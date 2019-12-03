{-|
 Module      : Data.Boltzmann.System.Utils
 Description : Various routines for combinatorial systems.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 The following module implements subroutines for combinatorial systems based
 on the excellent paper of Carine Pivoteau, Bruno Salvy, and Michèle Soria:

 Algorithms for combinatorial structures: Well-founded systems and Newton iterations.
 Journal of Combinatorial Theory, Series A 119 (2012) p. 1711–1773.
 -}
module Data.Boltzmann.System.Utils
    ( isEmptyAtZero
    , zeroCoordinates
    , wellFoundedAtZero
    , polynomial
    ) where

import           Prelude hiding ((<>))

import           Data.Map (Map)
import qualified Data.Map.Strict as M
import           Numeric.LinearAlgebra hiding (size)

import           Data.Boltzmann.System

-- | Non-strict multiplication.
(.*) :: (Num a, Eq a) => a -> a -> a
0 .* _ = 0
_ .* 0 = 0
1 .* x = x
x .* 1 = x
x .* y = x * y

-- | Evaluates the given system Y = H(Z, Y) at coordinates
--   Z = 0 and Y = 0 and checks if the outcome system is empty,
--   i.e. H(0, 0) = 0 or not.
isEmptyAtZero :: (Eq a, Num a) => System a -> Bool
isEmptyAtZero sys = all isZeroType ts
    where ts = M.toList $ defs sys

          isZeroType (_, consL) = all isZeroCons consL
          -- Note: A given constructor can give a non-zero coordinate
          --       iff it has no arguments and no positive weight. List
          --       arguments are irrelevant as they yield powers of unity.
          isZeroCons cons = not $ weight cons == 0 && null (args cons)

evalAtZero :: Num a => Arg -> a
evalAtZero (Type _) = 0
evalAtZero (List _) = 1

evalAtZeroL :: (Eq a, Num a) => [Arg] -> a
evalAtZeroL xs = foldl (.*) 1 $ map evalAtZero xs

derivTypeAtZero :: (Eq a, Eq b, Num a, Num b)
                => String -> [Cons b] -> a

derivTypeAtZero dt consL =
    sum $ map (derivConsAtZero dt) consL

derivConsAtZero :: (Eq a, Eq b, Num a, Num b)
                => String -> Cons a -> b

derivConsAtZero dt cons
    | weight cons /= 0 = 0
    | otherwise = derivConsAtZeroL dt (args cons)

derivConsAtZeroL :: (Eq a, Num a)
                 => String -> [Arg] -> a

derivConsAtZeroL _ [] = 1
derivConsAtZeroL dt (x:xs) = lhs + rhs
    where lhs = evalAtZero x .* derivConsAtZeroL dt xs
          rhs = derivAtZero dt x .* evalAtZeroL xs

derivAtZero :: Num a
            => String -> Arg -> a

derivAtZero dt arg
    | argName arg /= dt = 0
    | otherwise = 1

-- | Computes the Jacobian matrix of the given system
--   and evaluates it at zero-valued coordinates.
jacobian :: (Eq a, Num a) => System a -> Matrix Double
jacobian sys = (n><n) [idx i j | i <- [0..n-1], j <- [0..n-1]]
    where n = size sys
          ts = defs sys
          idx i j =
              let typ = snd $ M.elemAt i ts
                  dt  = fst $ M.elemAt j ts
                  in derivTypeAtZero dt typ

-- | Checks whether the given matrix is zero or not.
isZero :: Matrix Double -> Bool
isZero mat = all (== 0) $ concat (toLists mat)

-- | Checks whether the given Jacobian matrix nilpotent is or not.
isNilpotent :: Matrix Double -> Bool
isNilpotent mat = isZero $ power mat m
    where m = rows mat

-- | Squares the given matrix.
square :: Matrix Double -> Matrix Double
square m = m <> m

-- | Fast matrix exponentiation.
power :: Integral a
      => Matrix Double -> a -> Matrix Double

power m 1 = m
power m n
  | odd n = m <> square (power m $ n-1)
  | otherwise = square (power m $ n `div` 2)

data Coordinate = Zero | Z deriving (Eq)

-- | Maps coordinates into booleans.
coord :: Coordinate -> Bool
coord Zero = True
coord Z    = False

-- | Gives the constructor list of a given type.
constrList :: System a -> String -> [Cons a]
constrList sys = (M.!) (defs sys)

-- | Detects whether the given system, satisfying H(0,0) = 0
--   and having a nilpotent Jacobian matrix, admits zero coordinates
--   in its solution. See also the 0-coord subroutine of Pivoteau et al.
zeroCoordinates :: System a -> Bool
zeroCoordinates sys = zeroCoordinates' sys m f
    where f = M.fromSet (const Zero) $ types sys
          m = size sys

zeroCoordinates' :: (Eq b, Num b)
                 => System a -> b -> Map String Coordinate -> Bool

zeroCoordinates' _ 0 f = elem Zero $ M.elems f
zeroCoordinates' sys m f = zeroCoordinates' sys (m-1) f'
    where xs = map (\k -> (k, mapF $ constrList sys k)) (M.keys f)
          f' = M.fromAscList xs

          mapF consL
              | all (coord . mapF') consL = Zero
              | otherwise                 = Z

          mapF' cons
              | any (coord . mapF'') (args cons) = Zero
              | otherwise                        = Z

          mapF'' (List _) = Z
          mapF'' (Type t) =
              case t `M.lookup` f of
                  Just Zero -> Zero
                  _         -> Z

-- | Checks whether the given system is well-founded at zero. Note that
--   the input system is assumed to satisfy H(0,0) = 0. See also the
--   isWellFoundedAt0 subroutine of Pivoteau et al.
wellFoundedAtZero :: (Eq a, Num a)
                  => System a -> Bool

wellFoundedAtZero sys
    | isNilpotent (jacobian sys) = not $ zeroCoordinates sys
    | otherwise                  = False

-- | Yields an infinite stream of successive evaluation iterations
--   in form of Y[m+1] = H(Z, Y[m]); starting with the given Y[0] and z.
evals :: System Int -> Vector Double -> Double -> [Vector Double]
evals sys ys z = ys : evals sys (eval sys ys z) z

-- | Checks whether the given system, assumed to satisfy H(0,0) = 0
--   and having a nilpotent Jacobian, encodes an implicit polynomial
--   species. See also the isPolynomial subroutine of Pivoteau et al.
polynomial :: System Int -> Bool
polynomial sys = hs !! m  == hs !! (m+1)
    where m   = size sys
          hs  = evals sys vec 1 -- note: numerical evaluation.
          vec = vector $ replicate m 0
