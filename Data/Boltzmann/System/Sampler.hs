{-|
 Module      : Data.Boltzmann.System.Sampler
 Description : Sampler utilities for combinatorial systems.
 Copyright   : (c) Maciej Bendkowski, 2017-2020

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 -}
{-# LANGUAGE DeriveGeneric #-}
module Data.Boltzmann.System.Sampler
    ( Structure(..)
    , sampleStr
    , sampleStrIO
    ) where

import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Random
       (RandomGen(..), Rand, getRandomR, evalRandIO)

import GHC.Generics
import Data.Aeson

import Data.Map ((!))
import qualified Data.Map as M

import Data.Boltzmann.System

-- | General structure data type.
data Structure = Structure { name  :: String
                           , nodes :: [Structure]
                           } deriving (Generic, Show)

instance ToJSON Structure where
        toEncoding = genericToEncoding defaultOptions

data SamplerS =
        SamplerS { typeCons :: String -> [(Cons Double, Int)]
                 , typeProb :: String -> Double
                 }

-- | Prepares a general, parametrised system for sampling.
prepare :: PSystem Double -> SamplerS
prepare sys = SamplerS { typeCons = \s -> M.fromList (paramTypesW sys) ! s
                       , typeProb = typeWeight sys
                       }

randomP :: RandomGen g => MaybeT (Rand g) Double
randomP = lift (getRandomR (0, 1))

-- | Special list structure with auxiliary name.
listStr :: [Structure] -> Structure
listStr str = Structure {name = "[]", nodes = str}

genRandomArgs :: RandomGen g
              => SamplerS
              -> Int -> [Arg]
              -> MaybeT (Rand g) ([Structure], Int)

genRandomArgs sys ub (Type t : xs) =
    do guard (ub > 0)
       (arg, s)     <- genRandomStr' sys t ub
       (args', ub') <- genRandomArgs sys (ub - s) xs
       return (arg : args', s + ub')

genRandomArgs sys ub (List t : xs) =
    do guard (ub > 0)
       (arg, s)     <- genRandomStrL sys t ub
       (args', ub') <- genRandomArgs sys (ub -s) xs
       return (listStr arg : args', s + ub')

genRandomArgs _ _ [] = return ([], 0)

choose :: [(Cons Double, Int)]
       -> Double -> (String, [Arg], Int)

choose [(x,w)] _ = (func x, args x, w)

choose ((x,w):xs) p
    | p < weight x = (func x, args x, w)
    | otherwise    = choose xs p

choose _ _       = error "I wasn't expecting the Spanish inquisition!"

genRandomStr :: RandomGen g
             => PSystem Double
             -> String
             -> Int
             -> MaybeT (Rand g) (Structure, Int)

genRandomStr = genRandomStr' . prepare

sampleStr :: RandomGen g
          => PSystem Double
          -> String
          -> Int -> Int -> Rand g (Structure, Int)

sampleStr sys str lb ub = do
    sample <- runMaybeT (genRandomStr sys str ub)
    case sample of
        Nothing     -> sampleStr sys str lb ub
        Just (x, s) -> if lb <= s then return (x, s)
                                  else sampleStr sys str lb ub

sampleStrIO :: PSystem Double
            -> String
            -> Int -> Int
            -> IO (Structure, Int)

sampleStrIO sys str lb ub =
        evalRandIO $ sampleStr sys str lb ub

genRandomStr' :: RandomGen g
              => SamplerS
              -> String
              -> Int
              -> MaybeT (Rand g) (Structure, Int)

genRandomStr' sys str ub =
    do guard (ub > 0)
       p                       <- randomP
       let opts                =  typeCons sys str
       let (constr, args', w)  =  choose opts p
       (args'', w')            <- genRandomArgs sys (ub - w) args'
       return (Structure { name = constr
                         , nodes = args''}, w + w')

genRandomStrL :: RandomGen g
             => SamplerS
             -> String
             -> Int
             -> MaybeT (Rand g) ([Structure], Int)

genRandomStrL sys str ub =
    do guard (ub > 0)
       p <- randomP
       if p < typeProb sys str then
                               do (x, s)   <- genRandomStr' sys str ub
                                  (xs, s') <- genRandomStrL sys str (ub - s)
                                  return (x : xs, s + s')
                               else
                                  return ([], 0)
