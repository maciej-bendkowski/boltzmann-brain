-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Compiler
    ( Compiler(..)
    ) where

class Compiler a where
    compile :: a -> IO ()
