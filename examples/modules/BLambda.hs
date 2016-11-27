-- | Compiler: boltzmann-brain ALPHA (2016-11-27 12:16:28.171265 CET)
-- | Singularity: 5.09307861328125000000e-1
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
       if p < 0.509307861328125 then
         do (x0, w0) <- genRandomDeBruijn (ub - 1)
            return (S x0, 1 + w0)
         else return (Z, 2)

genRandomLambda ::
                  RandomGen g => Int -> MaybeT (Rand g) (Lambda, Int)
genRandomLambda ub
  = do guard (ub > 0)
       p <- randomP
       if p < 0.369454516996453 then
         do (x0, w0) <- genRandomLambda (ub - 2)
            (x1, w1) <- genRandomLambda (ub - 2 - w0)
            return (App x0 x1, 2 + w0 + w1)
         else
         if p < 0.6288490146070816 then
           do (x0, w0) <- genRandomLambda (ub - 2)
              return (Abs x0, 2 + w0)
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
