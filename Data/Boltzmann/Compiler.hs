-- |
-- Module      : Data.Boltzmann.Compiler
-- Description : Compiler configuration utilities.
-- Copyright   : (c) Maciej Bendkowski, 2017-2020
--
-- License     : BSD3
-- Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
-- Stability   : experimental
--
-- General framework of system compilers.
module Data.Boltzmann.Compiler
  ( Configuration (..),
  )
where

import Data.Boltzmann.System (PSystem)

-- | Compiler configurations.
class Configuration a where
  config ::
    -- | Parametrised system.
    PSystem Double ->
    -- | Module name.
    String ->
    -- | Compile note.
    String ->
    -- | Configuration.
    a

  compile ::
    -- | Configuration.
    a ->
    -- | Compiler IO action.
    IO ()
