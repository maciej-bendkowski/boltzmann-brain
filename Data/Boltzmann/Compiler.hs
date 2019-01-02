{-|
 Module      : Data.Boltzmann.Compiler
 Description : Compiler configuration utilities.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General framework of system compilers.
 -}
module Data.Boltzmann.Compiler
    ( Configuration(..)
    ) where

import Data.Boltzmann.System

-- | Compiler configurations.
class Configuration a where
    config  :: PSystem Double -- ^ Parametrised system.
            -> String         -- ^ Module name.
            -> String         -- ^ Compile note.
            -> a              -- ^ Configuration.

    compile :: a              -- ^ Configuration.
            -> IO ()          -- ^ Compiler IO action.
