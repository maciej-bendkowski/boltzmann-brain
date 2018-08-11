{-|
 Module      : Data.Boltzmann.System.Tuner.Utils
 Description : General interface utilities for Paganini.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Tuner.Utils
    ( PSolver(..)

    , PArg(..)
    , toArgs
    , getArgs
    , defaultArgs

    , PSpec(..)
    , Parametrisation(..)
    , parametrise
    ) where

import Data.Maybe

import qualified Data.Map.Strict as M
import Numeric.LinearAlgebra hiding (size,double)

import Data.Boltzmann.System

-- | Convex program solvers.
data PSolver = SCS
             | ECOS
             | CVXOPT

instance Show PSolver where
    show SCS    = "SCS"
    show ECOS   = "ECOS"
    show CVXOPT = "CVXOPT"

-- | Paganini arguments.
data PArg = PArg { solver    :: PSolver
                 , precision :: Double
                 , maxiters  :: Int
                 , sysType   :: SystemType
                 } deriving (Show)

toArgs :: PArg -> [String]
toArgs arg = ["--from-stdin"
             ,"-s", show (solver arg)
             ,"-p", show (precision arg)
             ,"-m", show (maxiters arg)
             ,"-t", show (sysType arg)]

rationalArgs :: PArg
rationalArgs = PArg { solver    = SCS
                    , precision = 1.0e-20
                    , maxiters  = 2500
                    , sysType   = Rational
                    }

algebraicArgs :: PArg
algebraicArgs = PArg { solver    = ECOS
                     , precision = 1.0e-20
                     , maxiters  = 20
                     , sysType   = Algebraic
                     }

-- | Determines default Paganini arguments.
--   Note: It is assumed that the given system is either
--   rational or algebraic. Otherwise, and error is raised.
defaultArgs :: System a -> PArg
defaultArgs sys =
    case systemType sys of
      Rational  -> rationalArgs
      Algebraic -> algebraicArgs
      _         -> error "Unsupported"

getArgs :: System Int -> Maybe PArg -> PArg
getArgs sys = fromMaybe (defaultArgs sys)

-- | Paganini helper specification.
data PSpec = PSpec { numFreqs    :: Int
                   , numTypes    :: Int
                   , numSeqTypes :: Int
                   }

-- | Parametrisation type.
data Parametrisation = Cummulative -- ^ Cummulative branching probabilities.
                     | Regular     -- ^ Independent probability masses.

-- | Compute the numerical branching probabilities for the given system.
parametrise :: System Int
            -> Parametrisation
            -> Double -> Vector Double
            -> [Double] -> PSystem Double

parametrise sys paramT rho ts us = PSystem { system  = computeProb sys paramT rho ts us
                                           , values  = ts
                                           , param   = rho
                                           , weights = sys
                                           }

evalExp :: System Int -> Double -> Vector Double
        -> [Double] -> Cons Int -> (Double, [Double])

evalExp sys rho ts us exp' =
    let w     = weight exp'
        xs    = args exp'
        exp'' = (rho ^^ w) * product (map (evalA sys ts) xs)
     in case frequency exp' of
          Nothing -> (exp'', us)
          Just _  -> (head us ^^ w * exp'', tail us)

computeExp :: System Int
           -> Parametrisation
           -> Double -> Vector Double
           -> [Double] -> Double -> Double -> [Cons Int]
           -> ([Cons Double], [Double])

computeExp _ _ _ _ us _ _ [] = ([], us)
computeExp sys Cummulative rho ts us tw w (e:es) = (e { weight = x / tw } : es', us'')
    where (es', us'') = computeExp sys Cummulative rho ts us' tw x es
          (w', us') = evalExp sys rho ts us e
          x = w + w'

computeExp sys Regular rho ts us tw _ (e:es) = (e { weight = w' / tw } : es', us'')
    where (es', us'') = computeExp sys Regular rho ts us' tw 0 es
          (w', us') = evalExp sys rho ts us e

computeProb' :: System Int
             -> Parametrisation
             -> Double -> Vector Double
             -> [Double] -> [(String, [Cons Int])]
             -> [(String, [Cons Double])]

computeProb' _ _ _ _ _ [] = []
computeProb' sys paramT rho ts us ((t,cons):tys) = (t,cons') : tys'
    where (cons', us') = computeExp sys paramT rho ts us (value t sys ts) 0.0 cons
          tys' = computeProb' sys paramT rho ts us' tys

computeProb :: System Int
            -> Parametrisation
            -> Double -> Vector Double
            -> [Double] -> System Double

computeProb sys paramT rho ts us = sys { defs = M.fromList tys }
    where tys = computeProb' sys paramT rho ts us (M.toList $ defs sys)
