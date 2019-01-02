{-|
 Module      : Data.Boltzmann.Internal.Tuner
 Description : General interface utilities for Paganini.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.Internal.Tuner
    ( PSolver(..)

    , PArg(..)
    , toArgs
    , getArgs
    , defaultArgs

    , PSpec(..)
    , Parametrisation(..)
    ) where

import Data.Maybe

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
                   } deriving (Show)

-- | Parametrisation type.
data Parametrisation = Cummulative -- ^ Cummulative branching probabilities.
                     | Regular     -- ^ Independent probability masses.
