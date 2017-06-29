-- | Compiler: Boltzmann brain v1.0
-- | Singularity: 0.2955970764160156
module Sampler
       (genRandomDeBruijn, genRandomLambda, genRandomDeBruijnList,
        genRandomLambdaList, sampleDeBruijn, sampleLambda,
        sampleDeBruijnList, sampleLambdaList, sampleDeBruijnIO,
        sampleLambdaIO, sampleDeBruijnListIO, sampleLambdaListIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Random
       (RandomGen(..), Rand, getRandomR, evalRandIO)

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
       if p < 0.35110353598417565 then
         do (x0, w0) <- genRandomLambda (ub - 1)
            (x1, w1) <- genRandomLambda (ub - 1 - w0)
            return (App x0 x1, 1 + w1 + w0)
         else
         if p < 0.6467006124001913 then
           do (x0, w0) <- genRandomLambda (ub - 1)
              return (Abs x0, 1 + w0)
           else
           do (x0, w0) <- genRandomDeBruijn ub
              return (Index x0, w0)

sampleDeBruijn :: RandomGen g => Int -> Int -> Rand g DeBruijn
sampleDeBruijn lb ub
  = do sample <- runMaybeT (genRandomDeBruijn ub)
       case sample of
           Nothing -> sampleDeBruijn lb ub
           Just (x, s) -> if lb <= s then return x else sampleDeBruijn lb ub

sampleLambda :: RandomGen g => Int -> Int -> Rand g Lambda
sampleLambda lb ub
  = do sample <- runMaybeT (genRandomLambda ub)
       case sample of
           Nothing -> sampleLambda lb ub
           Just (x, s) -> if lb <= s then return x else sampleLambda lb ub

sampleDeBruijnIO :: Int -> Int -> IO DeBruijn
sampleDeBruijnIO lb ub = evalRandIO (sampleDeBruijn lb ub)

sampleLambdaIO :: Int -> Int -> IO Lambda
sampleLambdaIO lb ub = evalRandIO (sampleLambda lb ub)
