-- | Compiler: boltzmann-brain ALPHA (2016-11-27 12:15:50.651482 CET)
-- | Singularity: 5.00000000000000000000e-1
module Sampler (B(..), genRandomB, sampleB) where
import Control.Monad (guard)
import Control.Monad.Random (RandomGen(..), Rand, getRandomR)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

data B = Leaf
       | Node B B
       deriving Show

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomB :: RandomGen g => Int -> MaybeT (Rand g) (B, Int)
genRandomB ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.5007072019510751 then return (Leaf, 1) else
         do (x0, w0) <- genRandomB (ub - 1)
            (x1, w1) <- genRandomB (ub - 1 - w0)
            return (Node x0 x1, 1 + w0 + w1)

sampleB :: RandomGen g => Int -> Int -> Rand g B
sampleB lb ub
  = do let sampler = runMaybeT (genRandomB ub)
       x <- sampler
       case x of
           Nothing -> sampleB lb ub
           Just (t', s) -> if lb <= s then return t' else sampleB lb ub
