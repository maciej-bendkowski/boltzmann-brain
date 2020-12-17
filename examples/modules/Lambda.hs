-- | Compiler:     Boltzmann Brain v1.6 (30-12-2019 18:01:59)
-- | Generated at: 30-12-2019 18:04:00
-- | Singularity:  0.295597742522085
-- | System:       (Types: 2, Constr: 5)
-- | System type:  algebraic
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell #-}
module Sampler
       (DeBruijn(), Lambda(), genRandomDeBruijn, genRandomLambda, sample,
        sampleDeBruijnIO, sampleLambdaIO)
       where
import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choice, runRIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Random (RandomGen(..))

data DeBruijn = S DeBruijn
              | Z deriving Show

data Lambda = Abs Lambda
            | App Lambda Lambda
            | Index DeBruijn
                deriving Show

decisionTreeDeBruijn :: DecisionTree Int
decisionTreeDeBruijn
  = $( TH.lift (decisionTree [0.295597742522085]) )

decisionTreeLambda :: DecisionTree Int
decisionTreeLambda
  = $(
      TH.lift (decisionTree [0.295597742522085, 0.35220112873895826]) )

genRandomDeBruijn ::
                    (RandomGen g) => Int -> MaybeT (BuffonMachine g) (DeBruijn, Int)
genRandomDeBruijn ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeDeBruijn)
       case n of
           0 -> do (x0, w0) <- genRandomDeBruijn (ub - 1)
                   return (S x0, 1 + w0)
           _ -> return (Z, 1)

genRandomLambda ::
                  (RandomGen g) => Int -> MaybeT (BuffonMachine g) (Lambda, Int)
genRandomLambda ub
  = do guard (ub > 0)
       n <- lift (choice decisionTreeLambda)
       case n of
           0 -> do (x0, w0) <- genRandomLambda (ub - 1)
                   return (Abs x0, 1 + w0)
           1 -> do (x0, w0) <- genRandomLambda (ub - 1)
                   (x1, w1) <- genRandomLambda (ub - 1 - w0)
                   return (App x0 x1, 1 + w1 + w0)
           _ -> do (x0, w0) <- genRandomDeBruijn ub
                   return (Index x0, w0)

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

sampleDeBruijnIO :: Int -> Int -> IO DeBruijn
sampleDeBruijnIO lb ub = runRIO (sample genRandomDeBruijn lb ub)

sampleLambdaIO :: Int -> Int -> IO Lambda
sampleLambdaIO lb ub = runRIO (sample genRandomLambda lb ub)
