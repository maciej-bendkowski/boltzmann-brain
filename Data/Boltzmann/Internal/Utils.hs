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
    ( quote
    , closest
    , bold
    , italic
    , underline
    , boldColor
    ) where

import System.Console.Pretty

-- | Single-quotes the given string.
quote :: String -> String
quote m = "'" ++ m ++ "'"

format :: (String -> String)
       -> String -> IO String

format f s = do
    inColor <- supportsPretty
    return $ if inColor then f s
                        else s

-- | Given a string, outputs its bold variant
--  (assuming that the terminal supports it)
bold :: String -> IO String
bold = format (style Bold)

-- | Given a string, outputs its italic variant
--  (assuming that the terminal supports it)
italic :: String -> IO String
italic = format (style Italic)

-- | Given a string, outputs its underlined variant
--  (assuming that the terminal supports it)
underline :: String -> IO String
underline = format (style Underline)

-- | Given a string, outputs its bold, colored variant
--  (assuming that the terminal supports it)
boldColor :: Color -> String -> IO String
boldColor c = format (style Bold . color c)

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
