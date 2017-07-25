{-|
 Module      : Data.Boltzmann.System.Jacobian
 Description : Jacobian matrix utilities.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Jacobian
    ( jacobian
    , wellFounded
    ) where

import Data.Boltzmann.System

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as S

import qualified Data.Map as M

import Numeric.LinearAlgebra hiding (size)

-- | Symbolic derivable expressions.
data Derivable = Product Derivable Derivable
               | Union Derivable Derivable
               | Func String Int
               | Seq String Int
               | Z Int

norm :: [Arg] -> (MultiSet String, MultiSet String)
norm args' = norm' args' S.empty S.empty

norm' :: [Arg] -> MultiSet String -> MultiSet String
      -> (MultiSet String, MultiSet String)

norm' (Type t : xs) ts ls = norm' xs (t `S.insert` ts) ls
norm' (List t : xs) ts ls = norm' xs ts (t `S.insert` ls)
norm' [] ts ls            = (ts, ls)

toDerivableL :: [Cons Int] -> Derivable
toDerivableL cons = foldl1 Union $ map toDerivable cons

toDerivable :: Cons Int -> Derivable
toDerivable con = Product z derivs'
    where z       = Z $ weight con
          derivs  = toDerivable' (norm $ args con)
          derivs' = foldl Product (Z 0) derivs

toDerivable' :: (MultiSet String, MultiSet String) -> [Derivable]
toDerivable' (ts, ls) = ts' ++ ls'
    where ts' = map (uncurry Func) $ S.toOccurList ts
          ls' = map (uncurry Seq) $ S.toOccurList ls

-- | Evaluates the derivable in the given coordinates.
evalD :: System Int -> String -> Double
      -> Vector Double -> Derivable -> Double

evalD _ _ z _ (Z n) = z ^^ n
evalD sys _ _ ys (Func t n) = evalA sys ys (Type t) ^^ n
evalD sys _ _ ys (Seq t n) = evalA sys ys (List t) ^^ n

evalD sys f z ys (Product x y) = xd * yd
    where xd = evalD sys f z ys x
          yd = evalD sys f z ys y

evalD sys f z ys (Union x y) = xd + yd
    where xd = evalD sys f z ys x
          yd = evalD sys f z ys y

-- | Computes the derivative of the given derivable
--   expression at the numerical coordinates.
deriv :: System Int -> String -> Double
      -> Vector Double -> Derivable -> Double

deriv _ _ _ _ (Z _) = 0
deriv sys f _ ys (Func t n)
  | f /= t = 0
  | otherwise = let x = evalA sys ys (Type t) in
                    fromIntegral n * x ^^ (n-1)

deriv sys f _ ys (Seq t n)
  | f /= t = 0
  | otherwise = let x = evalA sys ys (List t) in
                    fromIntegral n * x ^^ (n+1)

deriv sys f z ys (Product x y) = x' * yd + xd * y'
    where x' = deriv sys f z ys x
          y' = deriv sys f z ys y
          xd = evalD sys f z ys x
          yd = evalD sys f z ys y

deriv sys f z ys (Union x y) = x' + y'
    where x' = deriv sys f z ys x
          y' = deriv sys f z ys y

jacobian' :: System Int -> Double -> Vector Double -> Int -> Int -> Double
jacobian' sys z ys i j = deriv sys f z ys (toDerivableL cons)
    where f    = fst (M.elemAt j sys')
          cons = snd (M.elemAt i sys')
          sys' = defs sys

-- | Computes the Jacobian matrix of the
--   system at given numberical coordinates.
jacobian :: System Int -> Double -> Vector Double -> Matrix Double
jacobian sys z ys = let n = size sys in
                        (n><n) [jacobian' sys z ys i j | i <- [0..n-1],
                                                         j <- [0..n-1]]

square :: Matrix Double -> Matrix Double
square m = m <> m

-- | Fast matrix exponentiation.
power :: Matrix Double -> Int -> Matrix Double
power m 1 = m
power m n
  | odd n = m * square (power m $ n-1)
  | otherwise = square (power m $ n `div` 2)

-- | Determines whether the given system well-founded.
--   Note: The system is assumed to define no empty structures.
wellFounded :: System Int -> Bool
wellFounded sys = empty $ m `power` n
    where m = jacobian sys 0.0 $ n |> [0.0..]
          n = size sys

empty :: Matrix Double -> Bool
empty m = all (==0.0) $ concat (toLists m)
