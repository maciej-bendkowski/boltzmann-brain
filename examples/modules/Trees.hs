-- | Compiler:     Boltzmann Brain v1.6 (30-12-2019 18:01:59)
-- | Generated at: 30-12-2019 18:05:12
-- | Singularity:  0.25
-- | System:       (Types: 1, Constr: 1)
-- | System type:  algebraic
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell #-}
module Sampler
       (Tree(), genRandomTree, genRandomTreeList, sample, sampleTreeIO,
        sampleTreeListIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choice, runRIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Random (RandomGen(..))

newtype Tree = Node [Tree]
                 deriving Show

decisionTreeTree :: DecisionTree Int
decisionTreeTree = $( TH.lift (decisionTree []) )

decisionTreeListTree :: DecisionTree Int
decisionTreeListTree
  = $( TH.lift (decisionTree [0.500000000000001]) )

genRandomTree ::
                (RandomGen g) => Int -> MaybeT (BuffonMachine g) (Tree, Int)
genRandomTree ub
  = do guard (ub > 0)
       (x0, w0) <- genRandomTreeList (ub - 1)
       return (Node x0, 1 + w0)

genRandomTreeList ::
                    (RandomGen g) => Int -> MaybeT (BuffonMachine g) ([Tree], Int)
genRandomTreeList ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeListTree)
       case n of
           0 -> do (x, w) <- genRandomTree ub
                   (xs, ws) <- genRandomTreeList (ub - w)
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

sampleTreeIO :: Int -> Int -> IO Tree
sampleTreeIO lb ub = runRIO (sample genRandomTree lb ub)

sampleTreeListIO :: Int -> Int -> IO [Tree]
sampleTreeListIO lb ub = runRIO (sample genRandomTreeList lb ub)
