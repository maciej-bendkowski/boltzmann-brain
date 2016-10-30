-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module System.Boltzmann
    ( BoltzmannSystem(..)
    , typeList
    , paramTypes
    , paramTypes'
    , weightedTypes
    ) where

import Prelude hiding (replicate, zipWith, all, any)
import Data.Vector hiding (sum, map, zip, product)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import System

data BoltzmannSystem a b = BoltzmannSystem { system :: System b
                                           , values :: Vector b
                                           , parameter :: b
                                           , weights :: System Integer
                                           } deriving (Show)

typeList :: BoltzmannSystem a b -> [String]
typeList = S.toList . M.keysSet . defs . system

paramTypes :: BoltzmannSystem a b -> [(String, [Cons b])]
paramTypes = M.toList . defs . system

paramTypes' :: BoltzmannSystem a b -> [(String, [(Cons b, Integer)])]
paramTypes' sys = map (addW $ weights sys) xs
    where xs = paramTypes sys

addW sys (s, cons) = (s, zip cons ws)
    where ws = typeW sys s

typeW :: System Integer -> String -> [Integer]
typeW sys s = case s `M.lookup` defs sys of
    Just cons -> map weight cons 
    Nothing -> []

weightedTypes :: BoltzmannSystem a b -> [(String, [Cons Integer])]
weightedTypes = M.toList . defs . weights
