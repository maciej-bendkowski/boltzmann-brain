-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module BoltzmannSystem
    ( BoltzmannSystem(..)
    , typeList
    , paramTypes
    , weightedTypes
    ) where

import Prelude hiding (replicate, zipWith, all, any)
import Data.Vector hiding (sum, map, product)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System

data BoltzmannSystem a = BoltzmannSystem { system :: System a
                                         , values :: Vector a 
                                         , parameter :: a
                                         , weights :: System Integer
                                         } deriving (Show)

typeList :: BoltzmannSystem a -> [String]
typeList = S.toList . M.keysSet . defs . system

paramTypes :: BoltzmannSystem a -> [(String, [Cons a])]
paramTypes = M.toList . defs . system

weightedTypes :: BoltzmannSystem a -> [(String, [Cons Integer])]
weightedTypes = M.toList . defs . weights
