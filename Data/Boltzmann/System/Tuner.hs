{-|
 Module      : Data.Boltzmann.System.Tuner
 Description : Interface utilities with the Paganini tuner.
 Copyright   : (c) Maciej Bendkowski, 2017-2020

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities managing the IO interface between Boltzmann Brain
 and the Paganini tuner script.
 -}
module Data.Boltzmann.System.Tuner
  ( readPaganini
  , runPaganini
  )
where

import           Data.Void
import qualified Data.Map.Strict               as M

import           Numeric.LinearAlgebra   hiding ( size
                                                , double
                                                )

import           Text.Megaparsec

import           Data.Boltzmann.Internal.Logging
import           Data.Boltzmann.Internal.Parser
import           Data.Boltzmann.Internal.Tuner
import           Data.Boltzmann.System
import           Data.Boltzmann.System.Paganini
import qualified Data.Boltzmann.System.Tuner.Algebraic
                                               as A
import           Data.Boltzmann.Internal.Annotations
                                                ( withString )

type Parser = Parsec Void String

-- | Communicates with Paganini and collects the respective
--   tuning vector for the given system. If communication is not possible,
--   for instance due to the missing Paganini script, the current process
--   is terminated with an error message on the standard error output.
runPaganini
  :: System Int
  -> Parametrisation
  -> IO (Either (ParseErrorBundle String Void) (PSystem Double))

runPaganini sys paramT = do
  info "Running paganini..."
  let gen = withString (annotations sys) "generate" (initType sys)
  result <- toPaganini gen sys
  case result of
    Left  err                           -> fail' (show err)
    Right (rho, ts, us, tsDDGs, lsDDGs) -> do
      info "Computing branching probabilities..."
      let ts'  = fromList ts
      let sys' = parametrise sys paramT rho ts' us
      return $ Right sys'
        { system = (system sys') { typeDDGs = tsDDGs, seqDDGs = lsDDGs }
        }

-- | Parses the given input string as a Paganini tuning vector.
readPaganini
  :: System Int
  -> Parametrisation
  -> String
  -> IO (Either (ParseErrorBundle String Void) (PSystem Double))

readPaganini sys paramT f = do
  let spec = A.toPSpec sys
  pag <- parsePaganini spec f
  case pag of
    Left  err           -> return $ Left err
    Right (rho, us, ts) -> do
      let ts' = fromList ts
      return (Right $ parametrise sys paramT rho ts' us)

paganiniStmt :: PSpec -> Parser (Double, [Double], [Double])
paganiniStmt spec = do
  rho <- double
  us  <- parseN double $ numFreqs spec
  ts  <- parseN double $ numTypes spec
  return (rho, us, ts)

-- | Parses the given Paganini specification.
parsePaganini
  :: PSpec
  -> String
  -> IO
       ( Either
           (ParseErrorBundle String Void)
           (Double, [Double], [Double])
       )

parsePaganini spec = parseFromFile (paganiniStmt spec)

accumulateCons :: Double -> [Cons Double] -> [Cons Double]
accumulateCons _   []         = []
accumulateCons acc (con : xs) = con' : accumulateCons acc' xs
 where
  con' = con { weight = w + acc }
  acc' = acc + w
  w    = weight con

accumulateType :: (String, [Cons Double]) -> (String, [Cons Double])
accumulateType (t, cons) = (t, accumulateCons 0 cons)

accumulate :: PSystem Double -> PSystem Double
accumulate psys = psys { system = sys { defs = M.fromList types' } }
 where
  sys    = system psys
  typs   = M.toList (defs sys)
  types' = map accumulateType typs

-- | Compute the numerical branching probabilities for the given system.
parametrise
  :: System Int
  -> Parametrisation
  -> Double
  -> Vector Double
  -> [Double]
  -> PSystem Double

parametrise sys paramT rho ts us =
  let sys' = A.paramSystem sys rho ts us
  in  case paramT of
        Regular     -> sys'
        Cummulative -> accumulate sys'
