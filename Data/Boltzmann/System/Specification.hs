{-|
 Module      : Data.Boltzmann.System.Specification
 Description : Combinatorial specification utilities.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Specification
    ( Semiring(..)
    , Spec(..)
    , evalZ
    , deriv
    , typeSpec
    ) where

import Data.Boltzmann.System

infixr 8 @+
infixr 9 @.

-- | Semiring type class.
class Semiring a where
    (@+), (@.) :: a -> a -> a
    zero, one :: a

-- | Combinatorial specification by Pivoteau et al.
data Spec = Bud
          | Empty
          | Neutral
          | Z Int
          | Class String
          | Union Spec Spec
          | Product Spec Spec
          | Seq Spec
          deriving (Eq)

instance Semiring Spec where
    x @+ y = simplify $ Union x y
    x @. y = simplify $ graft x y
    one    = Neutral
    zero   = Empty

simplify :: Spec -> Spec
simplify (Union x y) =
    case (simplify x, simplify y) of
      (Empty,rs) -> rs
      (ls,Empty) -> ls
      (ls,rs)    -> Union ls rs
simplify (Product x y) =
    case (simplify x, simplify y) of
      (Empty,_)    -> Empty
      (_,Empty)    -> Empty
      (Neutral,rs) -> rs
      (ls,Neutral) -> ls
      (ls,rs)      -> Product ls rs
simplify (Seq x) =
    case simplify x of
      Empty -> Neutral
      xs    -> Seq xs
simplify x = x

-- | Evaluates the specification in zero.
evalZ :: Spec -> Spec
evalZ = simplify . evalZ'

evalZ' :: Spec -> Spec
evalZ' (Union x y)   = Union (evalZ' x) (evalZ' y)
evalZ' (Product x y) = Product (evalZ' x) (evalZ' y)
evalZ' (Seq x)       = Seq (evalZ' x)
evalZ' (Class _)     = Empty
evalZ' (Z _)         = Empty
evalZ' x             = x

-- | Compute the combinatorial derivative with respect to the given class.
deriv :: String -> Spec -> Spec
deriv d spec = simplify $ deriv' d spec

deriv' :: String -> Spec -> Spec
deriv' d (Union x y) = Union (deriv' d x) (deriv' d y)
deriv' d (Product x y) = Union (Product x' y) (Product y' x)
    where x' = deriv' d x
          y' = deriv' d y
deriv' d spec @ (Seq x) = Product spec $ Product x' spec
    where x' = deriv' d x
deriv' d (Class t)
    | d == t = Bud
    | otherwise = Empty
deriv' _ _ = Empty

argSpec :: Arg -> Spec
argSpec (Type t) = Class t
argSpec (List t) = Seq $ Class t

consSpec :: Cons Int -> Spec
consSpec con = foldl Product zk $ map argSpec (args con)
    where zk = if w > 0 then Z w else Neutral
          w = weight con

-- | Turns a given type into a corresponding combinatorial specification.
typeSpec :: [Cons Int] -> Spec
typeSpec cons = foldl1 Union $ map consSpec cons

graft :: Spec -> Spec -> Spec
graft (Union x y) spec   = Union (graft x spec) (graft y spec)
graft (Product x y) spec = Product (graft x spec) (graft y spec)
graft (Seq x) spec       = Seq (graft x spec)
graft Bud spec           = spec
graft x _                = x
