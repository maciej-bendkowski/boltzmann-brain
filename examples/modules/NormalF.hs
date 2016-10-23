-- | Compiler: boltzmann-brain ALPHA (2016-10-23 16:40:26.039624 CEST)
-- | Singularity: 3.33333015441894531250e-1
module NormalF
       (D(..), genRandomD, sampleD, M(..), genRandomM, sampleM, N(..),
        genRandomN, sampleN)
       where
import Control.Monad (guard)
import Control.Monad.Random (RandomGen(..), Rand(..), getRandomR)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

data D = S D
       | Z
       deriving Show

data M = App M N
       | Index D
       deriving Show

data N = Neutral M
       | Abs N
       deriving Show

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomD :: RandomGen g => Int -> MaybeT (Rand g) (D, Int)
genRandomD ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.33333301544189453 then
         do (x0, w0) <- genRandomD (ub - 1)
            return $! (S x0, 1 + w0)
         else return $! (Z, 1)

genRandomM :: RandomGen g => Int -> MaybeT (Rand g) (M, Int)
genRandomM ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.498861137602297 then
         do (x0, w0) <- genRandomM (ub - 1)
            (x1, w1) <- genRandomN (ub - 1 - w0)
            return $! (App x0 x1, 1 + w0 + w1)
         else
         do (x0, w0) <- genRandomD (ub - 0)
            return $! (Index x0, 0 + w0)

genRandomN :: RandomGen g => Int -> MaybeT (Rand g) (N, Int)
genRandomN ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.6666676510292284 then
         do (x0, w0) <- genRandomM (ub - 0)
            return $! (Neutral x0, 0 + w0)
         else
         do (x0, w0) <- genRandomN (ub - 1)
            return $! (Abs x0, 1 + w0)

sampleD :: RandomGen g => Int -> Int -> Rand g D
sampleD lb ub
  = do let sampler = runMaybeT (genRandomD ub)
       x <- sampler
       case x of
           Nothing -> sampleD lb ub
           Just (t', s) -> if lb <= s then return t' else sampleD lb ub

sampleM :: RandomGen g => Int -> Int -> Rand g M
sampleM lb ub
  = do let sampler = runMaybeT (genRandomM ub)
       x <- sampler
       case x of
           Nothing -> sampleM lb ub
           Just (t', s) -> if lb <= s then return t' else sampleM lb ub

sampleN :: RandomGen g => Int -> Int -> Rand g N
sampleN lb ub
  = do let sampler = runMaybeT (genRandomN ub)
       x <- sampler
       case x of
           Nothing -> sampleN lb ub
           Just (t', s) -> if lb <= s then return t' else sampleN lb ub
