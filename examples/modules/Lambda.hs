-- | Compiler: boltzmann-brain ALPHA (2016-11-27 12:17:58.617074 CET)
-- | Singularity: 2.95597076416015625000e-1
module Sampler
       (DeBruijn(..), genRandomDeBruijn, sampleDeBruijn, Lambda(..),
        genRandomLambda, sampleLambda)
       where
import Control.Monad (guard)
import Control.Monad.Random (RandomGen(..), Rand, getRandomR)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

data DeBruijn = S DeBruijn
              | Z
              deriving Show

data Lambda = App Lambda Lambda
            | Abs Lambda
            | Index DeBruijn
            deriving Show

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

genRandomDeBruijn ::
                    RandomGen g => Int -> MaybeT (Rand g) (DeBruijn, Int)
genRandomDeBruijn ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.2955970764160156 then
         do (x0, w0) <- genRandomDeBruijn (ub - 1)
            return (S x0, 1 + w0)
         else return (Z, 1)

genRandomLambda ::
                  RandomGen g => Int -> MaybeT (Rand g) (Lambda, Int)
genRandomLambda ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.3511035359841748 then
         do (x0, w0) <- genRandomLambda (ub - 1)
            (x1, w1) <- genRandomLambda (ub - 1 - w0)
            return (App x0 x1, 1 + w0 + w1)
         else
         if p < 0.6467006124001904 then
           do (x0, w0) <- genRandomLambda (ub - 1)
              return (Abs x0, 1 + w0)
           else
           do (x0, w0) <- genRandomDeBruijn (ub - 0)
              return (Index x0, 0 + w0)

sampleDeBruijn :: RandomGen g => Int -> Int -> Rand g DeBruijn
sampleDeBruijn lb ub
  = do let sampler = runMaybeT (genRandomDeBruijn ub)
       x <- sampler
       case x of
           Nothing -> sampleDeBruijn lb ub
           Just (t', s) -> if lb <= s then return t' else sampleDeBruijn lb ub

sampleLambda :: RandomGen g => Int -> Int -> Rand g Lambda
sampleLambda lb ub
  = do let sampler = runMaybeT (genRandomLambda ub)
       x <- sampler
       case x of
           Nothing -> sampleLambda lb ub
           Just (t', s) -> if lb <= s then return t' else sampleLambda lb ub
