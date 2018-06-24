{-|
 Module      : Data.Boltzmann.System.Warnings
 Description : Various warning handling utilities.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Warning utilities meant to deal with, skippable, well-foundness checks
 or other redundant sanity checks of the considered combinatorial system.
 -}
module Data.Boltzmann.System.Warnings
    ( SystemWarning
    , WarningMonad
    , warnings
    ) where

import Control.Monad.Except

import qualified Data.Map.Strict as M

import Data.Boltzmann.System

-- | Semantic system warnings.
data SystemWarning = NullCons String                -- Type name
                              String                -- Constructor name

instance Show SystemWarning where
    show (NullCons t con) = "[Warning] Invalid constructor '" ++ con
        ++ "' in type " ++ t ++ ": encountered a structure of size 0."

-- | Monadic warning handling wrapper.
type WarningMonad = Either SystemWarning

-- | Checks whether the given input system admits no warnings.
warnings :: System Int -> WarningMonad ()
warnings = nullCons

nullCons :: (Num a, Eq a) => System a -> WarningMonad ()
nullCons sys = mapM_ nullType (M.toList $ defs sys) `catchError` Left
    where nullType (t,cons) = mapM_ (nullCon t) cons

          nullCon :: (Num a, Eq a) => String -> Cons a -> WarningMonad ()
          nullCon t con
            | null (args con) && weight con == 0 =
                throwError $ NullCons t (func con)
            | otherwise = return ()
