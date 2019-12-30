-- | Compiler:     Boltzmann Brain v1.6 (30-12-2019 18:01:59)
-- | Generated at: 30-12-2019 18:06:19
-- | Singularity:  19.7108223320847
-- | System:       (Types: 2, Constr: 4)
-- | System type:  rational
-- | Stability:    experimental
{-# LANGUAGE TemplateHaskell #-}
module Sampler (sampleWord, startingState, sampleWordIO) where
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Buffon.Machine
       (BuffonMachine, DecisionTree(..), decisionTree, choice, runRIO)
import qualified Language.Haskell.TH.Syntax as TH
import System.Random (RandomGen(..))

type Symbol = String

weight :: Int -> Int
weight n
  = case n of
        0 -> 1
        1 -> 1
        _ -> 1

symbol :: Int -> Symbol
symbol n
  = case n of
        0 -> "a"
        1 -> "b"
        _ -> "c"

decisionTrees :: Int -> DecisionTree Int
decisionTrees s
  = case s of
        0 -> $( TH.lift (decisionTree [0.4285741987517959]) )
        _ -> $( TH.lift (decisionTree [0.9999980546407334]) )

isTerminal :: Int -> Bool
isTerminal s
  = case s of
        0 -> False
        _ -> True

transitionMatrix :: Int -> Int -> (Int, Int)
transitionMatrix n m
  = case n of
        0 -> case m of
                 0 -> (1, 0)
                 _ -> (0, 1)
        _ -> case m of
                 0 -> (0, 2)
                 _ -> (-1, -1)

genWord ::
          (RandomGen g) =>
          Int ->
            Int -> [Symbol] -> Int -> MaybeT (BuffonMachine g) ([Symbol], Int)
genWord ub s acc ct
  = if ub <= 0 && isTerminal s then return (acc, ct) else
      do n <- lift (choice (decisionTrees s))
         let (s', i) = transitionMatrix s n
         if s' < 0 then return (acc, ct) else
           do let (a, w) = (symbol i, weight i)
              genWord (ub - w) s' (a : acc) (w + ct)

sampleWord ::
             (RandomGen g) => Int -> Int -> Int -> BuffonMachine g [Symbol]
sampleWord lb ub s
  = do str <- runMaybeT (genWord lb s [] 0)
       case str of
           Nothing -> sampleWord lb ub s
           Just (w, n) -> if lb <= n && n <= ub then return w else
                            sampleWord lb ub s

startingState :: Int
startingState = 0

sampleWordIO :: Int -> Int -> Int -> IO [Symbol]
sampleWordIO lb ub s = runRIO (sampleWord lb ub s)
