{-|
 Module      : Data.Boltzmann.Internal.Utils
 Description : General utilities for boltzmann-brain.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities for boltzmann-brain.
 -}
module Data.Boltzmann.Internal.Utils
    ( closest
    ) where

-- | Given a non-empty list of distinct strings, finds the closest
--   (in terms of editing distance) string to the given one.
closest :: [String] -> String -> String
closest dic s = closest' (tail dists) (head dists)
    where dists = zip dic $ map (editDist s) dic

closest' :: Ord b =>  [(a, b)] -> (a, b) -> a
closest' [] (m, _) = m
closest' ((x,k):xs) (m, k')
    | k < k' = closest' xs (x, k)
    | otherwise = closest' xs (m, k')

-- | Levenshtein distance of two strings.
editDist :: String -> String -> Int
editDist xs ys = editDist' (reverse xs) (reverse ys)

editDist' :: String -> String -> Int
editDist' "" b = length b
editDist' a "" = length a
editDist' a @ (x:xs) b @ (y:ys)
    | x == y = editDist xs ys
    | otherwise = minimum [1 + editDist' a ys
                          ,1 + editDist' xs b
                          ,1 + editDist' xs ys
                          ]
