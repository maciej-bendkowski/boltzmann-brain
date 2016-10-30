-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Compiler
    ( Compilable(..)
    ) where

class Compilable a where
    compile :: a -> IO ()
