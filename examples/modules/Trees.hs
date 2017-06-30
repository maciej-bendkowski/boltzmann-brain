-- | Compiler: Boltzmann brain v1.1
-- | Singularity: 0.25
module Sampler
       (genRandomTree, genRandomTreeList, sampleTree, sampleTreeList,
        sampleTreeIO, sampleTreeListIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Random
       (RandomGen(..), Rand, getRandomR, evalRandIO)

newtype Tree = Node [Tree]
             deriving Show

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomTree :: RandomGen g => Int -> MaybeT (Rand g) (Tree, Int)
genRandomTree ub
  = do (x0, w0) <- genRandomTreeList (ub - 1)
       return (Node x0, 1 + w0)

genRandomTreeList ::
                    RandomGen g => Int -> MaybeT (Rand g) ([Tree], Int)
genRandomTreeList ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.4992937853107338 then
         do (x, w) <- genRandomTree ub
            (xs, ws) <- genRandomTreeList (ub - w)
            return (x : xs, w + ws)
         else return ([], 0)

sampleTree :: RandomGen g => Int -> Int -> Rand g Tree
sampleTree lb ub
  = do sample <- runMaybeT (genRandomTree ub)
       case sample of
           Nothing -> sampleTree lb ub
           Just (x, s) -> if lb <= s then return x else sampleTree lb ub

sampleTreeList :: RandomGen g => Int -> Int -> Rand g [Tree]
sampleTreeList lb ub
  = do sample <- runMaybeT (genRandomTreeList ub)
       case sample of
           Nothing -> sampleTreeList lb ub
           Just (x, s) -> if lb <= s then return x else sampleTreeList lb ub

sampleTreeIO :: Int -> Int -> IO Tree
sampleTreeIO lb ub = evalRandIO (sampleTree lb ub)

sampleTreeListIO :: Int -> Int -> IO [Tree]
sampleTreeListIO lb ub = evalRandIO (sampleTreeList lb ub)
