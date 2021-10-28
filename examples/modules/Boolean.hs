-- | Compiler:     Boltzmann Brain v2.0 (28-10-2021 19:43:25)
-- | Generated at: 28-10-2021 20:30:17
-- | Singularity:  0.173013128066611
-- | System type:  algebraic
-- | Target size:  100000
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
module Sampler
       (Fun(), Index(), genRandomFun, genRandomIndex, genRandomFunList,
        genRandomIndexList, sample, sampleFunIO, sampleIndexIO,
        sampleFunListIO, sampleIndexListIO)
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

data Fun = Variable Index
         | And Fun Fun
         | Or Fun Fun
         | Neg Fun
             deriving (Generic, ToJSON, FromJSON, Show)

data Index = Succ Index
           | Zero
               deriving (Generic, ToJSON, FromJSON, Show)

ddgFun :: Vector Int
ddgFun
  = $(
      [|fromList
  [2, 189, 4, 185, 6, 181, 8, 177, 10, 173, 12, 169, 14, 165, 16,
   158, 18, 154, 20, 150, 22, 146, 24, 142, 26, 141, 28, 137, 30, 133,
   32, 129, 34, 128, 36, 121, 38, 114, 40, 107, 42, 103, 44, 99, 46,
   95, 48, 91, 50, 87, 52, 80, 54, 76, 56, 72, 58, 68, 60, 67, 62, 66,
   64, 65, -4, -1, -4, -4, 70, 71, -3, -1, 74, 75, -4, -3, 78, 79, -4,
   -3, 82, 86, 84, 85, -2, -1, -3, 89, 90, -2, -1, 93, 94, -4, -1, 97,
   98, -3, -1, 101, 102, -4, -1, 105, 106, -4, -3, 109, 113, 111, 112,
   -2, -1, -3, 116, 120, 118, 119, -2, -1, -4, 123, 127, 125, 126, -3,
   -2, -3, -4, 131, 132, -3, -2, 135, 136, -2, -1, 139, 140, -4, -1,
   -4, 144, 145, -3, -1, 148, 149, -2, -1, 152, 153, -3, -2, 156, 157,
   -4, -3, 160, 164, 162, 163, -2, -1, -1, 167, 168, -2, -1, 171, 172,
   -3, -2, 175, 176, -4, -3, 179, 180, -4, -1, 183, 184, -3, -2, 187,
   188, -4, -1, 191, 192, -2, -1]|]
      )

ddgIndex :: Vector Int
ddgIndex
  = $(
      [|fromList
  [2, 96, 4, 95, 6, 94, 8, 93, 10, 92, 12, 91, 14, 90, 16, 89, 18,
   88, 20, 87, 22, 86, 24, 85, 26, 84, 28, 83, 30, 82, 32, 81, 34, 80,
   36, 79, 38, 78, 40, 77, 42, 76, 44, 75, 46, 74, 48, 73, 50, 72, 52,
   71, 54, 70, 56, 69, 58, 68, 60, 67, 62, 66, 64, 65, -2, -1, -1, -1,
   -1, -1, -2, -2, -1, -2, -1, -1, -2, -1, -2, -2, -1, -2, -1, -2, -1,
   -2, -2, -1, -2, -2, -2, -1, -1, -2, -1, -2, -2]|]
      )

decisionTreeListFun :: DecisionTree Int
decisionTreeListFun
  = $( TH.lift (decisionTree [0.505948272455738]) )

decisionTreeListIndex :: DecisionTree Int
decisionTreeListIndex
  = $( TH.lift (decisionTree [0.209209038182904]) )

genRandomFun ::
               (RandomGen g) => Int -> MaybeT (BuffonMachine g) (Fun, Int)
genRandomFun ub
  = do guard (ub > 0)
       n <- lift (choiceDDG ddgFun)
       case n of
           0 -> do (x0, w0) <- genRandomIndex ub
                   return (Variable x0, w0)
           1 -> do (x0, w0) <- genRandomFun (ub - 1)
                   (x1, w1) <- genRandomFun (ub - 1 - w0)
                   return (And x0 x1, 1 + w1 + w0)
           2 -> do (x0, w0) <- genRandomFun (ub - 1)
                   (x1, w1) <- genRandomFun (ub - 1 - w0)
                   return (Or x0 x1, 1 + w1 + w0)
           _ -> do (x0, w0) <- genRandomFun (ub - 1)
                   return (Neg x0, 1 + w0)

genRandomIndex ::
                 (RandomGen g) => Int -> MaybeT (BuffonMachine g) (Index, Int)
genRandomIndex ub
  = do guard (ub > 0)
       n <- lift (choiceDDG ddgIndex)
       case n of
           0 -> do (x0, w0) <- genRandomIndex (ub - 1)
                   return (Succ x0, 1 + w0)
           _ -> return (Zero, 1)

genRandomFunList ::
                   (RandomGen g) => Int -> MaybeT (BuffonMachine g) ([Fun], Int)
genRandomFunList ub
  = do guard (ub > 0)
       n <- lift (choiceDDG ddgFun)
       case n of
           0 -> do (x, w) <- genRandomFun ub
                   (xs, ws) <- genRandomFunList (ub - w)
                   return (x : xs, w + ws)
           _ -> return ([], 0)

genRandomIndexList ::
                     (RandomGen g) => Int -> MaybeT (BuffonMachine g) ([Index], Int)
genRandomIndexList ub
  = do guard (ub > 0)
       n <- lift (choiceDDG ddgIndex)
       case n of
           0 -> do (x, w) <- genRandomIndex ub
                   (xs, ws) <- genRandomIndexList (ub - w)
                   return (x : xs, w + ws)
           _ -> return ([], 0)

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

sampleFunIO :: Int -> Int -> IO Fun
sampleFunIO lb ub = runRIO (sample genRandomFun lb ub)

sampleIndexIO :: Int -> Int -> IO Index
sampleIndexIO lb ub = runRIO (sample genRandomIndex lb ub)

sampleFunListIO :: Int -> Int -> IO [Fun]
sampleFunListIO lb ub = runRIO (sample genRandomFunList lb ub)

sampleIndexListIO :: Int -> Int -> IO [Index]
sampleIndexListIO lb ub = runRIO (sample genRandomIndexList lb ub)
