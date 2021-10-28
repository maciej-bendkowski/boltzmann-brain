{-|
 Module      : Data.Boltzmann.Internal.Utils
 Description : General utilities for boltzmann-brain.
 Copyright   : (c) Maciej Bendkowski, 2017-2021

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities for boltzmann-brain.
 -}
module Data.Boltzmann.Internal.Utils
    ( quote
    , ensureLn
    , closest

    , bold
    , italic
    , underline
    , boldColor

    , csv

    , getTime

    , writeListLn
    , printer
    , showsList
    ) where

import System.IO

import System.Console.Pretty
import Text.EditDistance

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

getTime :: IO String
getTime = do
    now <- getCurrentTime
    timeZone <- getCurrentTimeZone
    let t = utcToLocalTime timeZone now
    return $ formatTime defaultTimeLocale "%d-%m-%Y %H:%M:%S" t

writeListLn :: Show a => Handle -> [a] -> IO ()
writeListLn h xs = hPutStrLn h (showsList xs)

printer :: (a -> String -> String) -> [a] -> String
printer _ [] = ""
printer f xs = foldl1 (\a b -> (a . (" " ++) . b))
                        (map f xs) ""

showsList :: Show a => [a]
          -> String

showsList = printer shows


-- | Produces a comma separated value (csv) representation
--   of the given list of string. Note that after each comma
--   there's placed an additional whitespace character.
csv :: [String] -> String
csv [] = ""
csv xs = foldl1 (\x y -> x ++ ", " ++ y) xs

-- | Given a string, ensures that it ends with
--   a newline character, appending it if needed.
ensureLn :: String -> String
ensureLn s
    | last s == '\n' = s
    | otherwise = unlines [s]

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
editDist = levenshteinDistance defaultEditCosts
