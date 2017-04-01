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

class Configuration a where
    compile :: a -> IO ()
