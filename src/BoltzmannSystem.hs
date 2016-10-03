-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module BoltzmannSystem
    ( BoltzmannSystem(..)
    , typeList
    , typePair
    , weightPair
    ) where

import Prelude hiding (replicate, zipWith, all, any)
import Data.Vector hiding (sum, map, product)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System

data BoltzmannSystem = BoltzmannSystem { system :: System Double
                                       , values :: Vector Double 
                                       , parameter :: Double
                                       , weights :: System Integer
                                       } deriving (Show)

typeList :: BoltzmannSystem -> [String]
typeList = S.toList . M.keysSet . defs . system

typePair :: BoltzmannSystem -> [(String, [Cons Double])]
typePair = M.toList . defs . system

weightPair :: BoltzmannSystem -> [(String, [Cons Integer])]
weightPair = M.toList . defs . weights
