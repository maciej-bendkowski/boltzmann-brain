{-|
 Module      : Data.Boltzmann.System.Tuner.Algebraic
 Description : Interface utilities with the Paganini tuner.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities managing the IO interface between Boltzmann Brain
 and the Paganini tuner script.
 -}
module Data.Boltzmann.System.Tuner.Algebraic
    ( writeSpecification
    , toPSpec
    ) where

import Control.Monad

import System.IO

import Data.Map (Map)
import qualified Data.Map.Strict as M

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as B
import qualified Data.Set as S

import Data.Maybe

import Data.Boltzmann.System
import Data.Boltzmann.Internal.Logging

import Data.Boltzmann.System.Tuner.Utils

writeListLn :: Show a => Handle -> [a] -> IO ()
writeListLn h xs = hPutStrLn h (showsList xs)

printer :: (a -> String -> String) -> [a] -> String
printer _ [] = ""
printer f xs = foldl1 (\a b -> (a . (" " ++) . b))
                        (map f xs) ""

showsList :: Show a => [a]
          -> String

showsList = printer shows

-- | Writes the system specification into the given
--   file handle. In paricular, to Paganini's standard
--   input handle.
writeSpecification :: System Int -> Handle -> IO ()
writeSpecification sys hout = do
    info "Writing system specification..."
    let freqs   = frequencies sys
    let seqs    = seqTypes sys
    let spec    = toPSpec sys

    -- # of equations and frequencies
    writeListLn hout [numTypes spec + numSeqTypes spec
                     ,length freqs]

    -- vector of frequencies
    writeListLn hout freqs

    -- type specifications
    let find' x = x `S.findIndex` M.keysSet (defs sys)
    foldM_ (typeSpecification hout find' spec) 0 (M.elems $ defs sys)

    -- sequence specifications
    mapM_ (seqSpecification hout find' spec) seqs

frequencies :: System Int -> [Double]
frequencies sys = concatMap (mapMaybe frequency)
    ((M.elems . defs) sys)

toPSpec :: System Int -> PSpec
toPSpec sys = PSpec { numFreqs    = d
                    , numTypes    = ts
                    , numSeqTypes = ss
                    }

   where ts = size sys
         d  = length $ frequencies sys
         ss = length $ seqTypes sys

typeSpecification :: Handle -> (String -> Int) -> PSpec
                  -> Int -> [Cons Int] -> IO Int

typeSpecification hout find' spec idx cons = do
    let n = length cons
    hPrint hout n -- # of constructors
    foldM (consSpecification hout find' spec) idx cons

consSpecification :: Handle -> (String -> Int) -> PSpec
                  -> Int -> Cons Int -> IO Int

consSpecification hout find' spec idx cons = do
    let (vec, idx') = consVec find' spec idx cons
    writeListLn hout vec -- constructor specification
    return idx'

indicator :: Int -> Int -> [Int]
indicator n k = indicator' n k 1

indicator' :: Int -> Int -> Int -> [Int]
indicator' 0 _ _ = []
indicator' n 0 x = x : replicate (n-1) 0
indicator' n k x = 0 : indicator' (n-1) (k-1) x

occurrences :: Cons a -> (MultiSet String, MultiSet String)
occurrences cons = occurrences' (B.empty,B.empty) $ args cons

occurrences' :: (MultiSet String, MultiSet String)
             -> [Arg] -> (MultiSet String, MultiSet String)

occurrences' (ts,sts) [] = (ts,sts)
occurrences' (ts,sts) (Type s : xs) = occurrences' (s `B.insert` ts,sts) xs
occurrences' (ts,sts) (List s : xs) = occurrences' (ts, s `B.insert` sts) xs

consVec :: (String -> Int) -> PSpec -> Int -> Cons Int -> ([Int], Int)
consVec find' spec idx cons =
      let (tocc, socc) = occurrences cons
          w            = fromIntegral $ weight cons
          dv           = indicator' (numFreqs spec) idx (weight cons)
          tv           = typeVec find' (numTypes spec) tocc
          sv           = typeVec find' (numSeqTypes spec) socc

      in case frequency cons of
           Just _  -> (w : dv ++ tv ++ sv, idx + 1)
           Nothing -> (w : replicate (numFreqs spec) 0 ++ tv ++ sv, idx)

typeVec :: (String -> Int) -> Int -> MultiSet String -> [Int]
typeVec find' size' m = typeVec' find' vec ls
    where vec = M.fromList [(n,0) | n <- [0..size'-1]]
          ls  = B.toOccurList m

typeVec' :: (String -> Int) -> Map Int Int -> [(String,Int)] -> [Int]
typeVec' _ vec [] = M.elems vec
typeVec' find' vec ((t,n) : xs) = typeVec' find' vec' xs
    where vec' = M.insert (find' t) n vec

seqSpecification :: Handle -> (String -> Int) -> PSpec
                 -> String -> IO ()

seqSpecification hout find' spec st = do
    let n = 1 + numTypes spec + numFreqs spec + numSeqTypes spec
    let f = replicate (numFreqs spec) 0
    let t = indicator (numTypes spec) (find' st)
    let s = indicator (numSeqTypes spec) (find' st)
    hPrint hout (2 :: Int) -- # of constructors
    writeListLn hout $ replicate n (0 :: Int)
    writeListLn hout $ 0 : f ++ t ++ s
