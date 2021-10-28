{-|
 Module      : Data.Boltzmann.Internal.TH
 Description : Template haskell utilities.
 Copyright   : (c) Maciej Bendkowski, 2017-2021

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General template haskell utilities.
 -}
module Data.Boltzmann.Internal.TH
    ( compileTime
    ) where

import Language.Haskell.TH

import Data.Boltzmann.Internal.Utils (getTime)

-- | Compile-time date.
compileTime :: Q Exp
compileTime = do
    t <- runIO getTime
    litE (StringL t)
