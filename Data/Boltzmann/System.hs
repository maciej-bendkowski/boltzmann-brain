{-|
 Module      : Data.Boltzmann.System 
 Description : System utilities for combinatorial specifications.
 Copyright   : (c) Maciej Bendkowski, 2017
 
 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System
    ( System(..)
    , size 
    , Cons(..)
    , Arg(..)
    , types
    , PSystem(..)
    , typeList
    , paramTypes
    , paramTypesW
    , typeWeight
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map.Strict as M

import Data.Vector (Vector)
import qualified Data.Vector as V

newtype System a = System { defs :: Map String [Cons a] 
                          } deriving (Show)

size :: System a -> Int
size = M.size . defs

data Cons a = Cons { func   :: String
                   , args   :: [Arg]
                   , weight :: a
                   } deriving (Show)

data Arg = Type String
         | List String
           deriving (Show)

types :: System a -> Set String
types = M.keysSet . defs

data PSystem a = PSystem { system  :: System a
                         , values  :: Vector a
                         , param   :: a
                         , weights :: System Int
                         } deriving (Show)

typeList :: PSystem a -> [String]
typeList = S.toList . M.keysSet . defs . system

paramTypes :: PSystem a -> [(String, [Cons a])]
paramTypes = M.toList . defs . system

paramTypesW :: PSystem a -> [(String, [(Cons a, Int)])]
paramTypesW sys = map (addW $ weights sys) xs
    where xs = paramTypes sys

addW :: System Int -> (String, [a]) -> (String, [(a, Int)])
addW sys (s, cons) = (s, zip cons ws)
    where ws = typeW sys s

typeW :: System Int -> String -> [Int]
typeW sys s = case s `M.lookup` defs sys of
    Just cons -> map weight cons 
    Nothing -> []

typeWeight :: PSystem a -> String -> a
typeWeight sys t = vec V.! idx
    where m   = defs $ system sys
          vec = values sys
          idx = M.findIndex t m
