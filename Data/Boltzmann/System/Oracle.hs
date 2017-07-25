{-|
 Module      : Data.Boltzmann.System.Oracle
 Description : Numeric Newton oracle utilities.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Oracle
    ( singularity
    , parametrise
    ) where

import qualified Data.Map as M

import Numeric.LinearAlgebra hiding (size)

import Data.Boltzmann.System
import Data.Boltzmann.System.Jacobian

computeProb :: System Int -> Double -> Vector Double -> System Double
computeProb sys z ys = sys { defs = M.mapWithKey computeProb' (defs sys) }
    where computeProb' t = computeExp (value t sys ys) 0.0
          computeExp _ _ [] = []
          computeExp tw w (e:es) = e { weight = x / tw } : computeExp tw x es
              where w' = evalC sys z ys e
                    x  = w' + w

-- | Compute the numerical Boltzmann probabilities for the given system.
parametrise :: System Int -> Double -> Double -> PSystem Double
parametrise sys rho eps = parametrise' initState state sys rho eps
    where initState = size sys |> [0..]
          state     = newton sys initState rho

parametrise' :: Vector Double -> Vector Double
             -> System Int -> Double -> Double -> PSystem Double

parametrise' state' state sys rho eps
  | not $ halt eps state' state =
      let newState = newton sys state rho
       in parametrise' state newState sys rho eps
  | otherwise = PSystem { system  = computeProb sys rho state
                        , values  = state
                        , param   = rho
                        , weights = sys
                        }

-- | Newton iteration for combinatorial systems.
newton :: System Int -> Vector Double -> Double -> Vector Double
newton sys state rho = state + (inv (ide - m) #> (h - state))
    where h         = eval sys state rho
          m         = jacobian sys rho state
          ide       = ident $ size sys

-- | Finds a numerical approximation of the system's dominating singularity.
singularity :: System Int -> Double -> Double
singularity sys eps = singularity' 0 1.0
    where singularity' lb ub
            | abs (ub - lb) < eps = lb
            | otherwise = if divergent sys eps z then singularity' lb z
                                                 else singularity' z ub
            where z = (ub + lb) / 2

divergent :: System Int -> Double -> Double -> Bool
divergent sys eps z = divergent' 0 state initState
    where initState = size sys |> [0..]
          state     = newton sys initState z

          divergent' :: Int -> Vector Double -> Vector Double -> Bool
          divergent' iter v v'
            | negative v || iter >= 25 = True
            | halt eps v v' = False
            | otherwise = divergent' (iter+1) (newton sys v z) v

negative :: Vector Double -> Bool
negative v = any (< 0) $ toList v

-- | Decide whether the system diverges or not.
halt :: Double -> Vector Double -> Vector Double -> Bool
halt eps v w = all (< eps) $ toList $ cmap abs (v - w)
