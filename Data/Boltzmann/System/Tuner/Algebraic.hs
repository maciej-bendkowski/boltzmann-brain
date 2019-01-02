{-|
 Module      : Data.Boltzmann.System.Tuner.Algebraic
 Description : Interface utilities with the Paganini tuner.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities managing the IO interface between Boltzmann Brain
 and the Paganini tuner script.
 -}
module Data.Boltzmann.System.Tuner.Algebraic
    ( writeSpecification
    , paramSystem
    , toPSpec
    ) where

import Control.Monad

import System.IO

import qualified Data.Map.Strict as M

import Numeric.LinearAlgebra hiding (size)

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as B
import qualified Data.Set as S

import Data.Maybe

import Data.Boltzmann.System
import Data.Boltzmann.Internal.Utils
import Data.Boltzmann.Internal.Tuner

-- | Writes the system specification into the given
--   file handle. In paricular, to Paganini's standard
--   input handle.
writeSpecification :: System Int -> Handle -> IO ()
writeSpecification sys hout = do
    let freqs   = frequencies sys
    let seqs    = seqTypes sys
    let spec    = toPSpec sys

    -- # of equations and frequencies
    writeListLn hout [numTypes spec + numSeqTypes spec, length freqs]

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
    let (vec, idx') = sparseConsVec find' spec idx cons
    writeListLn hout vec -- constructor specification
    return idx'

occurrences :: Cons a -> (MultiSet String, MultiSet String)
occurrences cons = occurrences' (B.empty,B.empty) $ args cons

occurrences' :: (MultiSet String, MultiSet String)
             -> [Arg] -> (MultiSet String, MultiSet String)

occurrences' (ts,sts) [] = (ts,sts)
occurrences' (ts,sts) (Type s : xs) = occurrences' (s `B.insert` ts,sts) xs
occurrences' (ts,sts) (List s : xs) = occurrences' (ts, s `B.insert` sts) xs

-- | Prepends a pair to a list if its
--   first component is positive.
sparsePrepend :: (Num a, Ord a)
              => (a, b) -> [(a, b)] -> [(a, b)]

sparsePrepend x xs
  | fst x > 0 = x : xs
  | otherwise = xs

prependWeight :: (Num a, Ord a, Num b)
              => Cons a -> [(a, b)] -> [(a, b)]

prependWeight cons =
    sparsePrepend (weight cons, 0)

prependFreq :: (Num a, Ord a, Num b)
            => Cons a -> b -> [(a, b)] -> [(a, b)]

prependFreq cons idx xs
  | isJust (frequency cons) =
      sparsePrepend (weight cons, 1 + idx) xs -- note: offset for z
  | otherwise = xs

sparseTypeVec :: Num a => (b -> a) -> a -> [(b, c)] -> [(c, a)]
sparseTypeVec _ _ [] = []
sparseTypeVec find' offset ((t,n) : xs) =
    (n, offset + find' t) : sparseTypeVec find' offset xs

sparseConsVec :: (String -> Int) -> PSpec
              -> Int -> Cons Int -> ([(Int,Int)], Int)

sparseConsVec find' spec idx cons =
    let (tocc, socc) = occurrences cons

        us = numFreqs spec
        -- sparse type representation
        tv = sparseTypeVec find' (1 + us) (B.toOccurList tocc)

        ts = numTypes spec
        -- sparse sequence representation
        sv = sparseTypeVec find' (1 + us + ts) (B.toOccurList socc)

        -- prepend weight and frequency
        xs = prependWeight cons (prependFreq cons idx $ tv ++ sv)

    in case frequency cons of
         Just _  -> (xs, idx + 1)
         Nothing -> (xs, idx)

seqSpecification :: Handle -> (String -> Int) -> PSpec
                 -> String -> IO ()

seqSpecification hout find' spec st = do
    hPrint hout (2 :: Int) -- # of constructors
    writeListLn hout ([] :: [Int]) -- empty sequence constructor
    let offset = 1 + numFreqs spec
    writeListLn hout [(1, offset + find' st) :: (Int,Int)
                     ,(1, offset + numTypes spec + find' st)] -- cons constructor

evalExp :: System Int -> Double -> Vector Double
        -> [Double] -> Cons Int -> (Double, [Double])

evalExp sys rho ts us exp' =
    let w     = weight exp'
        xs    = args exp'
        exp'' = (rho ^^ w) * product (map (evalA sys ts) xs)
     in case frequency exp' of
          Nothing -> (exp'', us)
          Just _  -> (head us ^^ w * exp'', tail us)

computeExp :: System Int
           -> Double -> Vector Double
           -> [Double] -> Double -> Double -> [Cons Int]
           -> ([Cons Double], [Double])

computeExp _ _ _ us _ _ [] = ([], us)
computeExp sys rho ts us tw _ (e:es) = (e { weight = w' / tw } : es', us'')
    where (es', us'') = computeExp sys rho ts us' tw 0 es
          (w', us')   = evalExp sys rho ts us e

computeProb :: System Int
            -> Double -> Vector Double
            -> [Double] -> [(String, [Cons Int])]
            -> [(String, [Cons Double])]

computeProb _ _ _ _ [] = []
computeProb sys rho ts us ((t,cons):tys) = (t,cons') : tys'
    where (cons', us') = computeExp sys rho ts us (value t sys ts) 0 cons
          tys'         = computeProb sys rho ts us' tys

paramSystem :: System Int
            -> Double -> Vector Double
            -> [Double] -> PSystem Double

paramSystem sys rho ts us = sys'
    where types'  = computeProb sys rho ts us (M.toList $ defs sys)
          sys'    = PSystem { system  = sys { defs = M.fromList types' }
                            , values  = ts
                            , param   = rho
                            , weights = sys
                            }
