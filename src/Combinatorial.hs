-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Combinatorial
    ( Spec(..)
    , evalZero
    , deriv
    , typeSpec
    ) where

import Semiring
import System

data Spec = Bud
          | Empty
          | Neutral
          | Z Integer
          | Class String
          | Union Spec Spec
          | Product Spec Spec
          | Seq Spec
          deriving (Show, Eq)

simplify :: Spec -> Spec
simplify (Union x y) = case (simplify x, simplify y) of
                         (Empty, r) -> r
                         (l, Empty) -> l
                         (l, r) -> Union l r

simplify (Product x y) = case (simplify x, simplify y) of
                           (Empty, r) -> Empty
                           (l, Empty) -> Empty
                           (Neutral, r) -> r
                           (l, Neutral) -> l
                           (l, r) -> Product l r

simplify (Seq x) = case simplify x of
                     Empty -> Neutral
                     x' -> Seq x'

simplify x = x

instance Semiring Spec where
    x @+ y = simplify $ Union x y
    x @. y = simplify $ graft x y
    one = Neutral
    zero = Empty

evalZero :: Spec -> Spec
evalZero = simplify . evalZero'

evalZero' :: Spec -> Spec
evalZero' (Union x y) = Union (evalZero' x) (evalZero' y)
evalZero' (Product x y) = Product (evalZero' x) (evalZero' y)
evalZero' (Seq x) = Seq (evalZero' x)
evalZero' (Class _) = Empty
evalZero' (Z _) = Empty
evalZero' x = x

deriv :: String -> Spec -> Spec
deriv d spec = simplify (deriv' d spec)

deriv' :: String -> Spec -> Spec
deriv' d (Union x y) = Union (deriv' d x) (deriv' d y)
deriv' d (Product x y) = Union (Product x' y) (Product y' x)
    where x' = deriv' d x
          y' = deriv' d y

deriv' d s @ (Seq x) = Product s (Product x' s)
    where x' = deriv' d x

deriv' d (Class t)
  | d == t = Bud
  | otherwise = Empty

deriv' _ _ = Empty

consSpec :: Cons Integer -> Spec
consSpec con = foldl Product zk (map argSpec $ args con)
    where zk = if w > 0 then Z w else Neutral
          w = weight con

argSpec :: Arg -> Spec
argSpec (Type t) = Class t
argSpec (List t) = Seq (Class t)

typeSpec :: [Cons Integer] -> Spec
typeSpec cons = foldl1 Union (map consSpec cons)

graft :: Spec -> Spec -> Spec
graft (Union x y) spec = Union (graft x spec) (graft y spec)
graft (Product x y) spec = Product (graft x spec) (graft y spec)
graft (Seq x) spec = Seq (graft x spec)
graft Bud spec = spec
graft x _ = x
