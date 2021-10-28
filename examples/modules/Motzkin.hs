-- | Compiler:     Boltzmann Brain v2.0 (28-10-2021 19:43:25)
-- | Generated at: 28-10-2021 20:29:58
-- | Singularity:  0.333333083333287
-- | System type:  algebraic
-- | Target size:  1000
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
module Sampler
       (M(), genRandomM, genRandomMList, sample, sampleMIO, sampleMListIO)
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

data M = Leaf
       | Unary M
       | Binary M M
           deriving (Generic, ToJSON, FromJSON, Show)

ddgM :: Vector Int
ddgM
  = $(
      [|fromList
  [2, 132, 4, 131, 6, 127, 8, 126, 10, 122, 12, 121, 14, 117, 16,
   116, 18, 112, 20, 108, 22, 104, 24, 103, 26, 99, 28, 98, 30, 97,
   32, 96, 34, 95, 36, 94, 38, 90, 40, 86, 42, 82, 44, 81, 46, 80, 48,
   79, 50, 78, 52, 74, 54, 70, 56, 66, 58, 65, 60, 64, 62, 63, -2, -1,
   -2, -3, 68, 69, -2, -1, 72, 73, -3, -2, 76, 77, -3, -2, -2, -2, -2,
   -3, 84, 85, -2, -1, 88, 89, -2, -1, 92, 93, -3, -1, -2, -3, -2, -1,
   -3, 101, 102, -2, -1, -3, 106, 107, -2, -1, 110, 111, -3, -1, 114,
   115, -2, -1, -3, 119, 120, -2, -1, -3, 124, 125, -2, -1, -3, 129,
   130, -2, -1, -3, 134, 135, -2, -1]|]
      )

decisionTreeListM :: DecisionTree Int
decisionTreeListM = $( TH.lift (decisionTree [0.998501123876053]) )

genRandomM ::
             (RandomGen g) => Int -> MaybeT (BuffonMachine g) (M, Int)
genRandomM ub
  = do guard (ub > 0)
       n <- lift (choiceDDG ddgM)
       case n of
           0 -> return (Leaf, 1)
           1 -> do (x0, w0) <- genRandomM (ub - 1)
                   return (Unary x0, 1 + w0)
           _ -> do (x0, w0) <- genRandomM (ub - 1)
                   (x1, w1) <- genRandomM (ub - 1 - w0)
                   return (Binary x0 x1, 1 + w1 + w0)

genRandomMList ::
                 (RandomGen g) => Int -> MaybeT (BuffonMachine g) ([M], Int)
genRandomMList ub
  = do guard (ub > 0)
       n <- lift (choiceDDG ddgM)
       case n of
           0 -> do (x, w) <- genRandomM ub
                   (xs, ws) <- genRandomMList (ub - w)
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

sampleMIO :: Int -> Int -> IO M
sampleMIO lb ub = runRIO (sample genRandomM lb ub)

sampleMListIO :: Int -> Int -> IO [M]
sampleMListIO lb ub = runRIO (sample genRandomMList lb ub)
