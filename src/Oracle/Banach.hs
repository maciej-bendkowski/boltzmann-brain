-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Oracle.Banach
    ( State(..)
    ) where

import Oracle
import System

import Data.Vector (Vector)
import qualified Data.Vector as V

newtype State a = State (Vector a)

instance (Fractional a, Ord a) => Oracle (State a) a where
    yield (State vec) = vec
    init sys z = State (V.replicate (size sys) 0.0)
    iterate sys z (State v) = State $ eval sys z v
