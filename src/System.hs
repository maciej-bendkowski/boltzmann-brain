-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module System
    ( System(..)
    , size 
    , Cons(..)
    , Arg(..)
    , types
    ) where

import qualified Data.Map.Strict as M

import Data.Set (Set)
import qualified Data.Set as S

data System a = System { defs :: M.Map String [Cons a] 
                       } deriving (Show)

size :: System a -> Int
size = M.size . defs

data Cons a = Cons { func :: String
                   , args :: [Arg]
                   , weight :: a
                   } deriving (Show)

data Arg = Type String
         | List String
           deriving (Show)

types :: System a -> Set String
types = M.keysSet . defs
