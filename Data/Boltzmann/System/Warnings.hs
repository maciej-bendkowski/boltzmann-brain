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
{-# LANGUAGE ExistentialQuantification #-}
module Data.Boltzmann.System.Warnings
    ( warnings
    ) where

import Control.Monad (unless)
import Data.Maybe (mapMaybe)

import System.Exit

import qualified Data.Map.Strict as M

import Data.Boltzmann.System

import Data.Boltzmann.Internal.Utils
import qualified Data.Boltzmann.Internal.Logging as L

-- | Semantic system warnings.
class SystemWarn a where

    -- | System warning message.
    report :: a -> String

-- | Existential warning type.
data WarningExt = forall a. (SystemWarn a) => WarningExt a

-- | Constructors without positive weight.
data ConsWeightWarn =
    ConsWeightWarn { consWeightType :: String -- ^ Type name.
                   , consWeightCons :: String -- ^ Constructor name.
                   }

consWeightWarn :: (Eq a, Num a) => System a -> [WarningExt]
consWeightWarn sys =
        concatMap nullType (M.toList $ defs sys)
    where
        nullCons typ cons
            | null (args cons) && weight cons == 0 =
                Just $ WarningExt ConsWeightWarn { consWeightType = typ
                                                 , consWeightCons = func cons
                                                 }
            | otherwise = Nothing
        nullType (typ, cons) = mapMaybe (nullCons typ) cons

instance SystemWarn ConsWeightWarn where
    report warn = "Found a constructor " ++ cons'
            ++ " in type " ++ typ' ++ " of weight 0."
        where
            cons' = quote $ consWeightCons warn
            typ'  = quote $ consWeightType warn

-- | Reports the given warning.
reportWarning :: WarningExt -> IO ()
reportWarning (WarningExt warn) = L.warn (report warn)

checkWarns :: Bool -> [WarningExt] -> IO ()
checkWarns werror warns = do
    mapM_ reportWarning warns
    unless (null warns || not werror)
        (exitWith $ ExitFailure 1)

-- | List of checked warnings.
warningList :: System Int -> [WarningExt]
warningList = consWeightWarn

-- | Checks whether the given input system admits no warnings.
warnings :: Bool -> System Int -> IO ()
warnings werror sys =  checkWarns werror (warningList sys)
