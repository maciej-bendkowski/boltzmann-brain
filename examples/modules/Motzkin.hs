-- | Compiler:     Boltzmann Brain v1.6 (30-12-2019 18:01:59)
-- | Generated at: 30-12-2019 18:05:00
-- | Singularity:  0.35
-- | System:       (Types: 1, Constr: 3)
-- | System type:  algebraic
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell #-}
module Sampler
       (M(), genRandomM, genRandomMList, sample, sampleMIO, sampleMListIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choice, runRIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Random (RandomGen(..))

data M = Leaf
       | Unary M
       | Binary M M
           deriving Show

decisionTreeM :: DecisionTree Int
decisionTreeM
  = $(
      TH.lift (decisionTree [0.3500000000000007, 0.30000000000000027]) )

decisionTreeListM :: DecisionTree Int
decisionTreeListM = $( TH.lift (decisionTree [0.999999999999998]) )

genRandomM ::
             (RandomGen g) => Int -> MaybeT (BuffonMachine g) (M, Int)
genRandomM ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeM)
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
       n <- lift (choice decisionTreeListM)
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
