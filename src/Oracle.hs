-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Oracle
    ( singularity
    , BoltzmannSystem
    , toBoltzmann
    , toBoltzmannS
    ) where

import Prelude hiding (replicate, zipWith, all, any)
import Data.Vector hiding (sum, map, product)

import qualified Data.Map.Strict as M

import System
import BoltzmannSystem

value :: String -> System b -> Vector a -> a
value t sys vec = vec ! M.findIndex t (defs sys)

singularity :: (Fractional a, Ord a, Integral b) => System b -> a -> a
singularity sys eps = singularity' 0 1.0
    where singularity' l u
            | abs (u - l) < eps = l
            | otherwise = if divergent sys eps z then singularity' l z
                                                 else singularity' z u
                    where z = (u + l) / 2

divergent :: (Fractional a, Ord a, Integral b) =>  System b -> a -> a -> Bool
divergent sys eps z = divergent' (iter sys z vec) vec
    where divergent' v v'
            | any (> 10) v = True -- (1.0/eps)) v = True
            | halt eps v v' = False
            | otherwise = divergent' (iter sys z v) v
          vec = replicate (size sys) 0.0

halt :: (Num a, Ord a) => a -> Vector a -> Vector a -> Bool
halt eps v v' = all (< eps) vec
    where vec = zipWith (\x y -> abs $ x-y) v v'

iter :: (Fractional a, Integral b) => System b -> a -> Vector a -> Vector a
iter sys z vec = imap update vec
    where update idx _ = sum (map evalE $ snd (M.elemAt idx $ defs sys))

          evalT (Type t) = value t sys vec
          evalE e = (z ^^ w) * product (map evalT ts)
              where w = weight e
                    ts = args e

toBoltzmann' :: (Fractional a, Ord a, Show a) => a -> a -> System Integer -> Vector a -> Vector a -> BoltzmannSystem a
toBoltzmann' sysEps rho sys v v'
  | not (halt sysEps v v') = toBoltzmann' sysEps rho sys (iter sys rho v) v
  | otherwise = BoltzmannSystem { system = parametrize sys rho v
                                , values = v
                                , parameter = rho
                                , weights = sys
                                }

toBoltzmann :: (Fractional a, Ord a, Show a) => System Integer -> a -> a -> BoltzmannSystem a
toBoltzmann sys singEps sysEps = toBoltzmann' sysEps rho sys (iter sys rho vec) vec
    where rho = singularity sys singEps
          vec = replicate (size sys) 0.0

toBoltzmannS :: (Fractional a, Ord a, Show a) => a -> System Integer -> t -> a -> BoltzmannSystem a
toBoltzmannS rho sys singEps sysEps = toBoltzmann' sysEps rho sys (iter sys rho vec) vec
    where vec = replicate (size sys) 0.0

parametrize :: (Fractional a, Integral b) => System b -> a -> Vector a -> System a
parametrize sys rho vec = sys { defs = M.mapWithKey parametrize' (defs sys) }
    where 
          parametrize' t = parametrizeE tw 0.0
              where tw = value t sys vec

          parametrizeE tw w [] = []
          parametrizeE tw w (e:es) = e { weight = x / tw } : parametrizeE tw x es
              where w' = evalCons sys rho vec e
                    x = w' + w

evalCons :: (Fractional a, Integral c) => System b -> a -> Vector a -> Cons c -> a
evalCons sys z vec e = (z ^^ w) * product (map (evalArg sys vec) ts)
    where w = weight e
          ts = args e

evalArg :: System b -> Vector a -> Arg -> a
evalArg sys vec (Type t) = value t sys vec
