{-|
 Module      : Data.Boltzmann.System.Tuner
 Description : Interface utilities with the Paganini tuner.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities managing the IO interface between Boltzmann Brain
 and the Paganini tuner script.
 -}
module Data.Boltzmann.System.Tuner
    ( readPaganini
    , runPaganini
    ) where

import Control.Exception

import System.IO
import System.Process hiding (system)

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Map.Strict as M
import Numeric.LinearAlgebra hiding (size,double)

import Data.Boltzmann.System
import Data.Boltzmann.Internal.Parser
import Data.Boltzmann.Internal.Logging
import Data.Boltzmann.Internal.Tuner
import Data.Boltzmann.Internal.Utils

import qualified Data.Boltzmann.System.Tuner.Algebraic as A
import qualified Data.Boltzmann.System.Tuner.Rational as R

-- | Catch IO exceptions.
try' :: IO a ->  IO (Either IOException a)
try' =  Control.Exception.try

-- | Communicates with Paganini and collects the respective
--   tuning vector for the given system. If communication is not possible,
--   for instance due to the missing Paganini script, the current process
--   is terminated with an error message on the standard error output.
runPaganini :: Format -> System Int -> Parametrisation -> Maybe PArg
            -> IO (Either (ParseError Char Dec)
                    (PSystem Double))

runPaganini sysFormat sys paramT arg = do

    info "Running paganini..."

    let arg' = getArgs sys arg
    info (printer (++) $ "Arguments: " : toArgs arg')

    -- Execute the paganini tuning script.
    pp <- try' $ createProcess (proc "medulla" (toArgs arg')){ std_out = CreatePipe
                                                              , std_in  = CreatePipe }

    case pp of
        Left _ -> fail' "Could not locate the medulla tuner. Is is available in the PATH?"
        Right (Just hin, Just hout, _, _) -> do

            -- write to paganini's stdout
            info "Writing system specification..."
            case sysFormat of
                RationalF  -> R.writeSpecification sys hin
                AlgebraicF -> A.writeSpecification sys hin

            -- read output parameters
            let spec = case sysFormat of
                          RationalF  -> R.toPSpec sys
                          AlgebraicF -> A.toPSpec sys

            s <- hGetContents hout
            let pag  = parse (paganiniStmt spec) "" s

            case pag of
              Left err -> return $ Left err
              Right (rho, us, ts) -> do
                  info "Parsing paganini output..."
                  let ts'  = fromList ts
                  let sys' = parametrise sysFormat sys paramT rho ts' us
                  return $ Right sys'

        _ -> fail' "Could not establish inter-process communication with medulla."

-- | Parses the given input string as a Paganini tuning vector.
readPaganini :: Format -> System Int -> Parametrisation -> String
             -> IO (Either (ParseError Char Dec)
                   (PSystem Double))

readPaganini sysFormat sys paramT f = do
    let spec = case sysFormat of
                   RationalF  -> R.toPSpec sys
                   AlgebraicF -> A.toPSpec sys

    pag <- parsePaganini spec f
    case pag of
        Left err -> return $ Left err
        Right (rho, us, ts) -> do
            let ts'  = fromList ts
            return (Right $ parametrise sysFormat sys paramT rho ts' us)

paganiniStmt :: PSpec -> Parser (Double, [Double], [Double])
paganiniStmt spec = do
    rho <- double
    us  <- parseN double $ numFreqs spec
    ts  <- parseN double $ numTypes spec
    return (rho, us, ts)

-- | Parses the given Paganini specification.
parsePaganini :: PSpec -> String
              -> IO (Either (ParseError Char Dec)
                    (Double, [Double], [Double]))

parsePaganini spec = parseFromFile (paganiniStmt spec)

accumulateCons :: Double -> [Cons Double] -> [Cons Double]
accumulateCons _ [] = []
accumulateCons acc (con : xs) =
    con' : accumulateCons acc' xs
    where con' = con { weight = w + acc }
          acc' = acc + w
          w    = weight con

accumulateType :: (String, [Cons Double]) -> (String, [Cons Double])
accumulateType (t, cons) =
    (t, accumulateCons 0 cons)

accumulate :: PSystem Double -> PSystem Double
accumulate psys = psys { system = sys { defs = M.fromList types' } }
    where sys    = system psys
          typs   = M.toList (defs sys)
          types' = map accumulateType typs

-- | Compute the numerical branching probabilities for the given system.
parametrise :: Format
            -> System Int
            -> Parametrisation
            -> Double -> Vector Double
            -> [Double] -> PSystem Double

parametrise sysFormat sys paramT rho ts us =
    let sys'     = paramFun sys rho ts us
        paramFun = case sysFormat of
                       RationalF  -> R.paramSystem
                       AlgebraicF -> A.paramSystem

        in case paramT of
            Regular     -> sys'
            Cummulative -> accumulate sys'
