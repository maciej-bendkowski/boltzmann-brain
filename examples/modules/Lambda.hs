-- | Compiler:     Boltzmann Brain v2.0 (28-10-2021 19:43:25)
-- | Generated at: 28-10-2021 20:31:12
-- | Singularity:  0.295597742522979
-- | System type:  algebraic
-- | Target size:  1000000
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
module Sampler
       (DeBruijn(), Lambda(), genRandomDeBruijn, genRandomLambda, sample,
        sampleDeBruijnIO, sampleLambdaIO)
       where
import GHC.Generics
import Data.Aeson
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choiceDDG, runRIO)
import Data.Vector (Vector(..), fromList)
import System.Random (RandomGen(..))

data DeBruijn = S DeBruijn
              | Z deriving (Generic, ToJSON, FromJSON, Show)

data Lambda = Abs Lambda
            | App Lambda Lambda
            | Index DeBruijn
                deriving (Generic, ToJSON, FromJSON, Show)

ddgDeBruijn :: Vector Int
ddgDeBruijn
  = $(
      [|fromList
  [2, 96, 4, 95, 6, 94, 8, 93, 10, 92, 12, 91, 14, 90, 16, 89, 18,
   88, 20, 87, 22, 86, 24, 85, 26, 84, 28, 83, 30, 82, 32, 81, 34, 80,
   36, 79, 38, 78, 40, 77, 42, 76, 44, 75, 46, 74, 48, 73, 50, 72, 52,
   71, 54, 70, 56, 69, 58, 68, 60, 67, 62, 66, 64, 65, -2, -1, -2, -1,
   -1, -2, -1, -2, -2, -1, -1, -2, -1, -2, -2, -1, -2, -2, -2, -1, -1,
   -2, -1, -2, -1, -1, -1, -2, -1, -2, -2, -1, -2]|]
      )

ddgLambda :: Vector Int
ddgLambda
  = $(
      [|fromList
  [2, 129, 4, 128, 6, 124, 8, 120, 10, 119, 12, 115, 14, 114, 16,
   113, 18, 112, 20, 108, 22, 107, 24, 103, 26, 102, 28, 101, 30, 97,
   32, 93, 34, 89, 36, 88, 38, 87, 40, 86, 42, 85, 44, 81, 46, 80, 48,
   79, 50, 78, 52, 77, 54, 76, 56, 72, 58, 68, 60, 67, 62, 66, 64, 65,
   -3, -1, -3, -3, 70, 71, -2, -1, 74, 75, -2, -1, -1, -3, -3, -1, -3,
   83, 84, -2, -1, -1, -2, -3, -3, 91, 92, -2, -1, 95, 96, -3, -2, 99,
   100, -3, -2, -1, -3, 105, 106, -2, -1, -3, 110, 111, -2, -1, -1,
   -1, -3, 117, 118, -2, -1, -3, 122, 123, -2, -1, 126, 127, -3, -2,
   -3, 131, 132, -2, -1]|]
      )

genRandomDeBruijn ::
                    (RandomGen g) => Int -> MaybeT (BuffonMachine g) (DeBruijn, Int)
genRandomDeBruijn ub
  = do guard (ub > 0)
       n <- lift (choiceDDG ddgDeBruijn)
       case n of
           0 -> do (x0, w0) <- genRandomDeBruijn (ub - 1)
                   return (S x0, 1 + w0)
           _ -> return (Z, 1)

genRandomLambda ::
                  (RandomGen g) => Int -> MaybeT (BuffonMachine g) (Lambda, Int)
genRandomLambda ub
  = do guard (ub > 0)
       n <- lift (choiceDDG ddgLambda)
       case n of
           0 -> do (x0, w0) <- genRandomLambda (ub - 1)
                   return (Abs x0, 1 + w0)
           1 -> do (x0, w0) <- genRandomLambda (ub - 1)
                   (x1, w1) <- genRandomLambda (ub - 1 - w0)
                   return (App x0 x1, 1 + w1 + w0)
           _ -> do (x0, w0) <- genRandomDeBruijn ub
                   return (Index x0, w0)

sample ::
         (RandomGen g) =>
         (Int -> MaybeT (BuffonMachine g) (a, Int)) ->
           Int -> Int -> BuffonMachine g a
sample gen lb ub
  = do str <- runMaybeT (gen ub)
       case str of
           Nothing -> sample gen lb ub
           Just (x, s) -> if lb <= s && s <= ub then return x else
                            sample gen lb ub

sampleDeBruijnIO :: Int -> Int -> IO DeBruijn
sampleDeBruijnIO lb ub = runRIO (sample genRandomDeBruijn lb ub)

sampleLambdaIO :: Int -> Int -> IO Lambda
sampleLambdaIO lb ub = runRIO (sample genRandomLambda lb ub)
