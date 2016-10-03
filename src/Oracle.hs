-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Oracle
    ( singularity
    , BoltzmannSystem
    , toBoltzmann
    ) where

import Prelude hiding (replicate, zipWith, all, any)
import Data.Vector hiding (sum, map, product)

import qualified Data.Map.Strict as M

import System
import BoltzmannSystem

value t sys vec = vec ! M.findIndex t (defs sys)

singularity :: Integral a => System a -> Double -> Double
singularity sys eps = singularity' 0 1.0
    where singularity' l u
            | abs (u - l) < eps = l
            | otherwise = if divergent sys eps z then singularity' l z
                                                 else singularity' z u
                    where z = (u + l) / 2

divergent :: Integral a => System a -> Double -> Double -> Bool
divergent sys eps z = divergent' (iter sys z vec) vec
    where divergent' v v'
            | any (> (1.0/eps)) v = True
            | halt eps v v' = False
            | otherwise = divergent' (iter sys z v) v
          vec = replicate (size sys) 0.0

halt :: (Num a, Ord a) => a -> Vector a -> Vector a -> Bool
halt eps v v' = all (< eps) vec
    where vec = zipWith (\x y -> abs $ x-y) v v'

iter :: Integral a => System a -> Double -> Vector Double -> Vector Double
iter sys z vec = imap update vec
    where update :: Int -> Double -> Double
          update idx _ = sum (map evalE $ snd (M.elemAt idx $ defs sys))

          evalT (Type t) = value t sys vec
          evalE e = (z ^^ w) * product (map evalT ts)
              where w = weight e
                    ts = args e

toBoltzmann :: System Integer -> Double -> BoltzmannSystem
toBoltzmann sys eps = toBoltzmann' (iter sys rho vec) vec
    where toBoltzmann' v v'
            | halt eps v v' = BoltzmannSystem { system = parametrize sys rho v
                                              , values = v
                                              , parameter = rho
                                              , weights = sys
                                              }
            | otherwise = toBoltzmann' (iter sys rho v) v
          
          rho = singularity sys eps
          vec = replicate (size sys) 0.0

parametrize sys rho vec = sys { defs = M.mapWithKey parametrize' (defs sys) }
    where 
          parametrize' t = parametrizeE tw 0.0
              where tw = value t sys vec

          parametrizeE tw w [] = []
          parametrizeE tw w (e:es) = e { weight = x / tw } : parametrizeE tw x es
              where w' = evalCons sys rho vec e
                    x = w' + w

evalCons sys z vec e = (z ^^ w) * product (map (evalArg sys vec) ts)
    where w = weight e
          ts = args e

evalArg sys vec (Type t) = value t sys vec
