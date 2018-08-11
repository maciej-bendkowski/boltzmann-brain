{-|
 Module      : Data.Boltzmann.System.Tuner
 Description : Interface utilities with the Paganini tuner.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

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

import Numeric.LinearAlgebra hiding (size,double)

import Data.Boltzmann.System
import Data.Boltzmann.Internal.Parser
import Data.Boltzmann.Internal.Logging

import Data.Boltzmann.System.Tuner.Utils
import qualified Data.Boltzmann.System.Tuner.Algebraic as A

-- | Catch IO exceptions.
try' :: IO a ->  IO (Either IOException a)
try' =  Control.Exception.try

printer :: (a -> String -> String) -> [a] -> String
printer _ [] = ""
printer f xs = foldl1 (\a b -> (a . (" " ++) . b))
                        (map f xs) ""

-- | Communicates with Paganini and collects the respective
--   tuning vector for the given system. If communication is not possible,
--   for instance due to the missing Paganini script, the current process
--   is terminated with an error message on the standard error output.
runPaganini :: System Int -> Parametrisation -> Maybe PArg
            -> IO (Either (ParseError Char Dec)
                    (PSystem Double))

runPaganini sys paramT arg = do

    info "Running paganini..."
    let arg' = getArgs sys arg
    info (printer (++) $ "Arguments: " : toArgs arg')

    -- Execute the paganini tuning script.
    pp <- try' $ createProcess (proc "paganini" (toArgs arg')){ std_out = CreatePipe
                                                              , std_in  = CreatePipe }

    case pp of
        Left _ -> fail' "Could not locate the paganini tuner. Is is available in the PATH?"
        Right (Just hin, Just hout, _, _) -> do

            -- write to paganini's stdout
            info "Writing system specification..."
            A.writeSpecification sys hin

            -- read output parameters
            s <- hGetContents hout
            let spec = A.toPSpec sys
            let pag  = parse (paganiniStmt spec) "" s

            case pag of
              Left err -> return $ Left err
              Right (rho, us, ts) -> do
                  info "Parsing paganini output..."
                  let ts'  = fromList ts
                  let sys' = parametrise sys paramT rho ts' us
                  return $ Right sys'

        _ -> fail' "Could not establish inter-process communication with paganini."

-- | Parses the given input string as a Paganini tuning vector.
readPaganini :: System Int -> Parametrisation -> String
             -> IO (Either (ParseError Char Dec)
                   (PSystem Double))

readPaganini sys paramT f = do
    let spec = A.toPSpec sys
    pag <- parsePaganini spec f
    case pag of
        Left err -> return $ Left err
        Right (rho, us, ts) -> do
            let ts'  = fromList ts
            return (Right $ parametrise sys paramT rho ts' us)

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
