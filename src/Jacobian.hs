-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Jacobian
    ( Jacobian(..)
    , jacobian
    , power
    , wellFounded
    ) where

import Semiring
import Combinatorial
import System

import qualified Data.Map as M
import Data.Array

type Jacobian i a = Array (i,i) a

jacobian :: System Integer -> Jacobian Int Spec
jacobian sys = array ((0,0),(m-1,m-1)) contents
    where contents = [((i,j), find sys i j) | i <- [0..m-1], j <- [0..m-1]]
          m = size sys

find :: System Integer -> Int -> Int -> Spec
find sys i j = evalZero $ deriv d (typeSpec cons)
    where cons = snd (M.elemAt i sys')
          d = fst (M.elemAt j sys')
          sys' = defs sys

(.@.) :: (Ix i, Semiring a) => Jacobian i a -> Jacobian i a -> Jacobian i a 
(.@.) x y = array ((x0,y1),(x0',y1')) contents
  where
    (((x0,x1),(x0',x1')), ((y0,y1),(y0',y1'))) = (bounds x, bounds y)
    (is, js, ks) = (range (x0,x0'), range (y1,y1'), range (x1,x1'))
    contents  = [((i,j), foldl1 (@+) [(x!(i,k)) @. (y!(k,j)) | k <- ks]) | i <- is, j <- js]

square :: (Ix i, Semiring a) => Jacobian i a -> Jacobian i a
square matrix = matrix .@. matrix

power :: (Ix i, Semiring a) => Int -> Jacobian i a -> Jacobian i a
power 1 matrix = matrix
power n matrix
    | odd n = matrix .@. square (power (n-1) matrix)
    | otherwise = square $ power (n `div` 2) matrix

wellFounded :: System Integer -> Bool
wellFounded sys = isEmpty jacP
    where jac = jacobian sys
          jacP = power m jac
          m = size sys

isEmpty :: Ix i => Jacobian i Spec -> Bool
isEmpty matrix = all (== Empty) arr
    where arr = elems matrix
