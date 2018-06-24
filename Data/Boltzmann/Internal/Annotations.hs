{-|
 Module      : Data.Boltzmann.Internal.Annotations
 Description : System annotation utilities.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities meant for handing system annotations
 guiding the tuning and compilation process.
 -}
module Data.Boltzmann.Internal.Annotations
    ( withDefault
    , withDouble
    , withInt
    , withBool
    ) where

import Data.Maybe (fromMaybe)
import Data.Char (toLower)

import Data.Map (Map)
import qualified Data.Map as M

import Text.Read (readMaybe)

-- | Read a given key value of a map with a default fallback.
withDefault :: Read a
            => Map String String
            -> String -> a -> a

withDefault f x d = case x `M.lookup` f of
                      Nothing -> d
                      Just x' -> fromMaybe d (readMaybe x')

-- | `withDefault` specialised to doubles.
withDouble :: Map String String
           -> String -> Double -> Double

withDouble = withDefault

-- | `withDefault` specialised to ints.
withInt :: Map String String
        -> String -> Int -> Int

withInt = withDefault

-- | `withDefault` specialised to bools.
withBool :: Map String String
         -> String -> Bool -> Bool

withBool f x d = case x `M.lookup` f of
                   Nothing -> d
                   Just x' -> case map toLower x' of
                                "yes"   -> True    -- support 'yes'
                                "y"     -> True    -- .. and alternative 'y'
                                "1"     -> True    -- .. and alternative '1'
                                "true"  -> True
                                "no"    -> False
                                "n"     -> False
                                "false" -> False
                                "0"     -> False
                                _       -> d       -- error, fall back to default
