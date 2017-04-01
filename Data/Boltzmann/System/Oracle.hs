{-|
 Module      : Data.Boltzmann.System.Oracle 
 Description : Numeric oracle utilities.
 Copyright   : (c) Maciej Bendkowski, 2017
 
 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Oracle 
    ( singularity
    , parametrise
    ) where

import Prelude hiding (init,iterate,exp)

import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Map.Strict as M

import Data.Boltzmann.System

value :: String -> System b -> Vector a -> a
value t sys vec = vec V.! M.findIndex t (defs sys)

eval :: (Fractional b, Integral a) => System a -> Vector b -> b -> Vector b
eval sys vec z = V.imap update vec
    where update idx _ = sum $ map (evalExp sys vec z) (f idx)
          f n          = snd $ M.elemAt n $ defs sys
         
evalType :: (Fractional a, Integral b) => System b -> Vector a -> Arg -> a
evalType sys vec (List t) = 1 / (1 - value t sys vec)
evalType sys vec (Type t) = value t sys vec

evalExp :: (Fractional a, Integral b) => System b -> Vector a -> a -> Cons b -> a
evalExp sys vec z exp = (z ^^ w) * product (map (evalType sys vec) ts)
    where w  = weight exp
          ts = args exp

computeProb :: (Fractional a, Integral b) => System b -> a -> Vector a -> System a
computeProb sys z vec = sys { defs = M.mapWithKey computeProb' (defs sys) }
    where computeProb' t = computeExp (value t sys vec) 0.0
          computeExp _ _ [] = []
          computeExp tw w (e:es) = e { weight = x / tw } : computeExp tw x es
              where w' = evalExp sys vec z e
                    x  = w' + w

-- | Compute the numerical Boltzmann probabilities for the given system.
parametrise :: (Fractional a, Ord a) => System Int -> a -> a -> PSystem a
parametrise sys rho eps = parametrise' initState state sys rho eps
    where initState = V.replicate (size sys) 0.0
          state     = eval sys initState rho

parametrise' :: (Fractional a, Ord a) => Vector a -> Vector a -> System Int -> a -> a -> PSystem a
parametrise' state' state sys rho eps
    | not (halt eps state' state) = 
            let newState = eval sys state rho
                in parametrise' state newState sys rho eps
    | otherwise = PSystem { system = computeProb sys rho state
                          , values = state
                          , param = rho
                          , weights = sys
                          }

-- | Finds a numerical approximation of the system's dominating singularity.
singularity :: (Fractional a, Integral b, Ord a) => System b -> a -> a
singularity sys eps = singularity' 0 1.0
    where singularity' lb ub
            | abs (ub - lb) < eps = lb
            | otherwise = if divergent sys eps z then singularity' lb z
                                                 else singularity' z ub
                    where z = (ub + lb) / 2

divergent :: (Fractional a, Integral b, Ord a) => System b -> a -> a -> Bool
divergent sys eps z = divergent' (eval sys vec z) vec
    where vec = V.replicate (size sys) 0.0
          divergent' v v'
            | V.any (> 1.5) v = True    -- Note: 1.0 admits numerical issues.
            | halt eps v v' = False
            | otherwise = divergent' (eval sys v z) v

halt :: (Num a, Ord a) => a -> Vector a -> Vector a -> Bool
halt eps v v' = V.all (< eps) vec
    where vec = V.zipWith (\x y -> abs $ x - y) v v'
