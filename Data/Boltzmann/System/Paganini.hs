{-|
 Module      : Data.Boltzmann.System.Paganini
 Description : Interface utilities with the Paganini tuner.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Paganini
    ( PSpec(..)
    , toPSpec
    , paganiniSpecification
    , parametrise
    ) where

import qualified Data.Set as S

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as B

import Data.Map (Map)
import qualified Data.Map.Strict as M

import Control.Monad
import Data.Maybe (mapMaybe)

import Numeric.LinearAlgebra hiding (size)

import Data.Boltzmann.System

frequencies :: System Int -> [Double]
frequencies sys = concatMap frequenciesT ((M.elems . defs) sys)

frequenciesT :: [Cons a] -> [Double]
frequenciesT = mapMaybe frequency

-- | Paganini helper specification.
data PSpec = PSpec { numFreqs    :: Int
                   , numTypes    :: Int
                   , numSeqTypes :: Int
                   }

toPSpec :: System Int -> PSpec
toPSpec sys = PSpec { numFreqs    = d
                    , numTypes    = ts
                    , numSeqTypes = ss
                    }
    where ts = size sys
          d  = length $ frequencies sys
          ss = length $ seqTypes sys

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

printListLn :: Show a => [a] -> IO ()
printListLn = putStrLn . showsList

showsList :: Show a => [a] -> String
showsList [] = ""
showsList xs = foldl1 (\a b -> (a . (" " ++) . b)) (map shows xs) ""

consSpecification :: (String -> Int) -> PSpec -> Int -> Cons Int -> IO Int
consSpecification find' spec idx cons = do
    let (vec, idx') = consVec find' spec idx cons
    printListLn vec
    return idx'

typeSpecification :: (String -> Int) -> PSpec -> Int -> [Cons Int] -> IO Int
typeSpecification find' spec idx cons = do
    let n = length cons
    print n -- # of constructors
    foldM (consSpecification find' spec) idx cons

seqSpecification :: (String -> Int) -> PSpec -> String -> IO ()
seqSpecification find' spec st = do
    let n = 1 + numTypes spec + numFreqs spec + numSeqTypes spec
    let f = replicate (numFreqs spec) 0
    let t = indicator (numTypes spec) (find' st)
    let s = indicator (numSeqTypes spec) (find' st)
    print (2 :: Int) -- # of constructors
    printListLn $ replicate n (0 :: Int)
    printListLn $ 0 : f ++ t ++ s

-- | Generates a paganini input specification of the given system.
paganiniSpecification :: System Int -> IO ()
paganiniSpecification sys = do
    let find' x = x `S.findIndex` M.keysSet (defs sys)
    let freqs  = frequencies sys
    let seqs   = seqTypes sys
    let spec   = toPSpec sys
    printListLn [numTypes spec + numSeqTypes spec, length freqs] -- # of equations and frequencies
    printListLn freqs                                            -- vector of frequencies
    foldM_ (typeSpecification find' spec) 0 (M.elems $ defs sys)
    mapM_ (seqSpecification find' spec) seqs

-- | Compute the numerical Boltzmann probabilities for the given system.
parametrise :: System Int -> Double -> Vector Double -> [Double] -> PSystem Double
parametrise sys rho ts us = PSystem { system  = computeProb sys rho ts us
                                    , values  = ts
                                    , param   = rho
                                    , weights = sys
                                    }

evalExp :: System Int -> Double -> Vector Double
        -> [Double] -> Cons Int -> (Double, [Double])

evalExp sys rho ts us exp' =
    let w     = weight exp'
        xs    = args exp'
        exp'' = (rho ^^ w) * product (map (evalA sys ts) xs)
     in case frequency exp' of
          Nothing -> (exp'', us)
          Just _  -> (head us ^^ w * exp'', tail us)

computeExp :: System Int -> Double -> Vector Double
           -> [Double] -> Double -> Double -> [Cons Int]
           -> ([Cons Double], [Double])

computeExp _ _ _ us _ _ [] = ([], us)
computeExp sys rho ts us tw w (e:es) = (e { weight = x / tw } : es', us'')
    where (es', us'') = computeExp sys rho ts us' tw x es
          (w', us') = evalExp sys rho ts us e
          x = w + w'

computeProb' :: System Int -> Double -> Vector Double
             -> [Double] -> [(String, [Cons Int])]
             -> [(String, [Cons Double])]

computeProb' _ _ _ _ [] = []
computeProb' sys rho ts us ((t,cons):tys) = (t,cons') : tys'
    where (cons', us') = computeExp sys rho ts us (value t sys ts) 0.0 cons
          tys' = computeProb' sys rho ts us' tys

computeProb :: System Int -> Double -> Vector Double -> [Double] -> System Double
computeProb sys rho ts us = sys { defs = M.fromList tys }
    where tys = computeProb' sys rho ts us (M.toList $ defs sys)
