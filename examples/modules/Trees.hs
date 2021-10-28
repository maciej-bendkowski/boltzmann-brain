-- | Compiler:     Boltzmann Brain v2.0 (28-10-2021 19:43:25)
-- | Generated at: 28-10-2021 20:31:55
-- | Singularity:  0.249999851843161
-- | System type:  algebraic
-- | Target size:  650
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
module Sampler
       (Tree(), genRandomTree, genRandomTreeList, sample, sampleTreeIO,
        sampleTreeListIO)
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

newtype Tree = Node [Tree]
                 deriving (Generic, ToJSON, FromJSON, Show)

ddgTree :: Vector Int
ddgTree = $( [|fromList []|] )

decisionTreeListTree :: DecisionTree Int
decisionTreeListTree
  = $( TH.lift (decisionTree [0.49961508852964]) )

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
       n <- lift (choiceDDG ddgTree)
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
