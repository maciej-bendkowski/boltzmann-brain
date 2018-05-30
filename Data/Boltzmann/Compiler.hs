{-|
 Module      : Data.Boltzmann.Compiler
 Description : Compiler configuration utilities.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.Compiler
    ( Configuration(..)
    ) where

import Data.Boltzmann.System

-- | Compiler configurations.
class Configuration a where
    config  :: PSystem Double -- ^ parametrised system.
            -> Maybe String   -- ^ output file location.
            -> String         -- ^ module name.
            -> String         -- ^ compile note.
            -> a              -- ^ configuration.

    compile :: a              -- ^ configuration.
            -> IO ()          -- ^ compiler action.
