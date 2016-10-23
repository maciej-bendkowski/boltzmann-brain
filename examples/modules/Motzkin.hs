-- | Compiler: boltzmann-brain ALPHA (2016-10-23 16:40:15.410196 CEST)
-- | Singularity: 3.33333015441894531250e-1
module Motzkin (M(..), genRandomM, sampleM) where
import Control.Monad (guard)
import Control.Monad.Random (RandomGen(..), Rand(..), getRandomR)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

data M = Leaf
       | Unary M
       | Binary M M
       deriving Show

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomM :: RandomGen g => Int -> MaybeT (Rand g) (M, Int)
genRandomM ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.3341408333975353 then return $! (Leaf, 1) else
         if p < 0.6674738488394298 then
           do (x0, w0) <- genRandomM (ub - 1)
              return $! (Unary x0, 1 + w0)
           else
           do (x0, w0) <- genRandomM (ub - 1)
              (x1, w1) <- genRandomM (ub - 1 - w0)
              return $! (Binary x0 x1, 1 + w0 + w1)

sampleM :: RandomGen g => Int -> Int -> Rand g M
sampleM lb ub
  = do let sampler = runMaybeT (genRandomM ub)
       x <- sampler
       case x of
           Nothing -> sampleM lb ub
           Just (t', s) -> if lb <= s then return t' else sampleM lb ub
