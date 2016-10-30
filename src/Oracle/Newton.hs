-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle.Newton
    ( State(..)
    ) where

import System
import Oracle hiding (eval)

import Combinatorial
import Jacobian

import Data.Matrix
import qualified Data.Map as M
import qualified Data.Array as A

import Data.Vector (Vector)
import qualified Data.Vector as V

evalSpec :: Fractional a => System b -> a -> Vector a -> Spec -> a
evalSpec sys z ys (Union x y) = evalSpec sys z ys x + evalSpec sys z ys y
evalSpec sys z ys (Product x y) = evalSpec sys z ys x * evalSpec sys z ys y
evalSpec sys z ys (Class c) = value c sys ys 
evalSpec sys z ys (Z n) = z ^^ n
evalSpec sys z ys Empty = 0
evalSpec sys z ys _ = 1

evalSys :: Fractional a => System Integer -> a -> Vector a -> Vector a
evalSys sys z ys = V.fromList es
    where ts = M.elems $ defs sys
          es = map (evalSpec sys z ys . typeSpec) ts

evalJacobian :: Fractional a => System b -> a -> Vector a -> Jacobian Int Spec -> Matrix a
evalJacobian sys z ys j = matrix (length ys) (length ys) 
    (\(a,b) -> evalSpec sys z ys (j A.! (a-1,b-1)))

(.+.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.+.) = elementwise (+)

(.-.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.-.) = elementwise (-)

(.*.) :: Num a => Matrix a -> Matrix a -> Matrix a
(.*.) = multStd

infixr 8 .+.
infixr 8 .-.
infixr 9 .*.

iterateM :: Fractional a => System b -> a -> State a -> Matrix a
iterateM sys z state = u .+. (u .*. (u .*. x) .-. (u .-. i))
    where u = us state
          y = ys state
          j = jac state
          x = evalJacobian sys z y j
          i = identity (size sys)

(|+|) :: Num a => V.Vector a -> V.Vector a -> V.Vector a
(|+|) = V.zipWith (+)

(|-|) :: Num a => V.Vector a -> V.Vector a -> V.Vector a
(|-|) = V.zipWith (-)

infixr 8 |+|
infixr 8 |-|

iterateN :: Fractional a => System Integer -> a -> State a -> (V.Vector a, Matrix a)
iterateN sys z state = (y |+| getMatrixAsVector (u' .*. colVector (h |-| y)), u')
    where u' = iterateM sys z state
          h = evalSys sys z y
          y = ys state

data State a = State { ys :: V.Vector a
                     , us :: Matrix a
                     , jac :: Jacobian Int Spec }

instance Fractional a => Oracle (State a) a where
    yield = ys
    
    iterate sys z state = let (ys', us') = iterateN sys z state in
        state { ys = ys'
              , us = us'
              }
    
    init sys z = State { ys = V.replicate (size sys) 0.0
                       , us = identity (size sys)
                       , jac = jacobian sys
                       }
