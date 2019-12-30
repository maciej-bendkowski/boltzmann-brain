-- | Compiler:     Boltzmann Brain v1.6 (30-12-2019 18:01:59)
-- | Generated at: 30-12-2019 18:03:16
-- | Singularity:  0.173015595712479
-- | System:       (Types: 2, Constr: 6)
-- | System type:  algebraic
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell #-}
module Boolean
       (Fun(), Index(), genRandomFun, genRandomIndex, genRandomFunList,
        genRandomIndexList, sample, sampleFunIO, sampleIndexIO,
        sampleFunListIO, sampleIndexListIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choice, runRIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Random (RandomGen(..))

data Fun = Variable Index
         | And Fun Fun
         | Or Fun Fun
         | Neg Fun
             deriving Show

data Index = Succ Index
           | Zero
               deriving Show

decisionTreeFun :: DecisionTree Int
decisionTreeFun
  = $(
      TH.lift
        (decisionTree
           [0.4134922021437745, 0.3259523393568867, 8.753986278686217e-2])
      )

decisionTreeIndex :: DecisionTree Int
decisionTreeIndex
  = $( TH.lift (decisionTree [0.17301559571247896]) )

decisionTreeListFun :: DecisionTree Int
decisionTreeListFun
  = $( TH.lift (decisionTree [0.505965155490016]) )

decisionTreeListIndex :: DecisionTree Int
decisionTreeListIndex
  = $( TH.lift (decisionTree [0.209212646351584]) )

genRandomFun ::
               (RandomGen g) => Int -> MaybeT (BuffonMachine g) (Fun, Int)
genRandomFun ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeFun)
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
       n <- lift (choice decisionTreeIndex)
       case n of
           0 -> do (x0, w0) <- genRandomIndex (ub - 1)
                   return (Succ x0, 1 + w0)
           _ -> return (Zero, 1)

genRandomFunList ::
                   (RandomGen g) => Int -> MaybeT (BuffonMachine g) ([Fun], Int)
genRandomFunList ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeListFun)
       case n of
           0 -> do (x, w) <- genRandomFun ub
                   (xs, ws) <- genRandomFunList (ub - w)
                   return (x : xs, w + ws)
           _ -> return ([], 0)

genRandomIndexList ::
                     (RandomGen g) => Int -> MaybeT (BuffonMachine g) ([Index], Int)
genRandomIndexList ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeListIndex)
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
