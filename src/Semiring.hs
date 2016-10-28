-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Semiring
    ( Semiring(..)
    ) where

infixr 8 @+
infixr 9 @.

class Semiring a where
    (@+), (@.) :: a -> a -> a
    zero, one :: a
