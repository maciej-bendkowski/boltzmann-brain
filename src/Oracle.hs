-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle
    ( Oracle(..)
    , singularity
    , toBoltzmann
    , toBoltzmannS
    , eval
    , value
    ) where

import System
import System.Boltzmann

import Prelude hiding (iterate, init)

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

class (Fractional b, Ord b) => Oracle a b where
    iterate :: System Integer -> b -> a -> a
    init :: System Integer -> b -> a
    yield :: a -> Vector b

value :: String -> System b -> Vector a -> a
value t sys vec = vec V.! M.findIndex t (defs sys)

singularity :: (Fractional a, Ord a) => System Integer -> a -> a
singularity sys eps = singularity' 0 1.0
    where singularity' l u
            | abs (u - l) < eps = l
            | otherwise = if divergent sys eps z then singularity' l z
                                                 else singularity' z u
                    where z = (u + l) / 2

divergent :: (Fractional a, Ord a) =>  System Integer -> a -> a -> Bool
divergent sys eps z = divergent' (eval sys z vec) vec
    where divergent' v v'
            | V.any (> 1.0/eps) v = True
            | halt eps v v' = False
            | otherwise = divergent' (eval sys z v) v
          vec = V.replicate (size sys) 0.0

eval :: Fractional a => System Integer -> a -> Vector a -> Vector a
eval sys z vec = V.imap update vec
    where update idx _ = sum (map evalE $ snd (M.elemAt idx $ defs sys))
          evalT (Type t) = value t sys vec
          evalE e = (z ^^ w) * product (map evalT ts)
              where w = weight e
                    ts = args e

halt :: (Num a, Ord a) => a -> Vector a -> Vector a -> Bool
halt eps v v' = V.all (< eps) vec
    where vec = V.zipWith (\x y -> abs $ x-y) v v'

parametrize :: Fractional a => System Integer -> a -> Vector a -> System a
parametrize sys rho vec = sys { defs = M.mapWithKey parametrize' (defs sys) }
    where parametrize' t = parametrizeE tw 0.0
              where tw = value t sys vec

          parametrizeE tw w [] = []
          parametrizeE tw w (e:es) = e { weight = x / tw } : parametrizeE tw x es
              where w' = evalCons sys rho vec e
                    x = w' + w

evalCons :: Fractional a => System Integer -> a -> Vector a -> Cons Integer -> a
evalCons sys z vec e = (z ^^ w) * product (map (evalArg sys vec) ts)
    where w = weight e
          ts = args e

evalArg :: System Integer -> Vector a -> Arg -> a
evalArg sys vec (Type t) = value t sys vec

toBoltzmann' :: Oracle a b => b -> b -> System Integer -> a -> a -> BoltzmannSystem a b
toBoltzmann' sysEps rho sys s s'
  | not (halt sysEps (yield s) (yield s')) = toBoltzmann' sysEps rho sys (iterate sys rho s) s
  | otherwise = BoltzmannSystem { system = parametrize sys rho (yield s)
                                , values = yield s
                                , parameter = rho
                                , weights = sys
                                }

toBoltzmann :: Oracle a b => System Integer -> b -> b -> BoltzmannSystem a b
toBoltzmann sys singEps sysEps = toBoltzmann' sysEps rho sys s s'
    where rho = singularity sys singEps
          s = iterate sys rho s'
          s' = init sys rho

toBoltzmannS :: Oracle a b => System Integer -> b -> b -> BoltzmannSystem a b
toBoltzmannS sys rho sysEps = toBoltzmann' sysEps rho sys s s'
    where s = iterate sys rho s'
          s' = init sys rho
