{-|
 Module      : Data.Boltzmann.System.Jacobian
 Description : Jacobian matrix utilities.
 Copyright   : (c) Maciej Bendkowski, 2017
 
 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Jacobian
    ( Jacobian
    , jacobian
    , wellFounded
    ) where

import Data.Boltzmann.System
import Data.Boltzmann.System.Specification

import Data.Array
import qualified Data.Map as M

-- | Combinatorial Jacobian matrix.
type Jacobian idx a = Array (idx,idx) a

-- | Jacobian matrix for the given combinatorial system. 
jacobian :: System Int -> Jacobian Int Spec
jacobian sys = array ((0,0),(m-1,m-1)) contents
    where contents = [((i,j), find sys i j) | i <- [0..m-1], j <- [0..m-1]]
          m        = size sys

find :: System Int -> Int -> Int -> Spec
find sys i j = evalZ $ deriv d (typeSpec cons)
    where cons = snd (M.elemAt i sys')
          d    = fst (M.elemAt j sys')
          sys' = defs sys

(.@.) :: (Ix idx, Semiring a) => Jacobian idx a -> Jacobian idx a -> Jacobian idx a 
(.@.) x y = array ((x0,y1),(x0',y1')) contents
  where
    (((x0,x1),(x0',x1')), ((_,y1),(_,y1'))) = (bounds x, bounds y)
    (is, js, ks)                            = (range (x0,x0'), range (y1,y1'), range (x1,x1'))
    contents                                = [((i,j), foldl1 (@+) [(x!(i,k)) @. (y!(k,j)) | k <- ks]) | i <- is, j <- js]

square :: (Ix idx, Semiring a) => Jacobian idx a -> Jacobian idx a
square matrix = matrix .@. matrix

power :: (Ix idx, Semiring a) => Int -> Jacobian idx a -> Jacobian idx a
power 1 matrix = matrix
power n matrix
    | odd n     = matrix .@. square (power (n-1) matrix)
    | otherwise = square $ power (n `div` 2) matrix

-- | Is the system well-founded?
--   Note: The system is assumed to define no structures of size zero.
wellFounded :: System Int -> Bool
wellFounded sys = isEmpty jacP
    where jac  = jacobian sys
          jacP = power m jac
          m    = size sys

isEmpty :: Ix idx => Jacobian idx Spec -> Bool
isEmpty matrix = all (== Empty) arr
    where arr = elems matrix
