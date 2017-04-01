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

-- | System of combinatorial structures.
newtype System a = System { defs :: Map String [Cons a]   -- ^ Type definitions. 
                          } deriving (Show)

size :: System a -> Int
size = M.size . defs

-- | Type constructor.
data Cons a = Cons { func   :: String       -- ^ Constructor name.
                   , args   :: [Arg]        -- ^ Argument list.
                   , weight :: a            -- ^ Constructor weight.
                   } deriving (Show)

-- | Type constructor arguments.
data Arg = Type String                      -- ^ Regular type reference.
         | List String                      -- ^ Type list reference.
           deriving (Show)

-- | Type set of the given system.
types :: System a -> Set String
types = M.keysSet . defs

-- | Parametrised system of combinatorial structures.
data PSystem a = PSystem { system  :: System a      -- ^ System with probability weights.
                         , values  :: Vector a      -- ^ Numerical values of corresponding types.
                         , param   :: a             -- ^ Evaluation parameter.
                         , weights :: System Int    -- ^ System with input weights.
                         } deriving (Show)

-- | Type list of the given parametrised system.
typeList :: PSystem a -> [String]
typeList = S.toList . M.keysSet . defs . system

-- | List of types with coresponding constructors.
paramTypes :: PSystem a -> [(String, [Cons a])]
paramTypes = M.toList . defs . system

-- | List of types with coresponding constructors and input weights.
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

-- | Type weight of the given parametrised system.
typeWeight :: PSystem a -> String -> a
typeWeight sys t = vec V.! idx
    where m   = defs $ system sys
          vec = values sys
          idx = M.findIndex t m
