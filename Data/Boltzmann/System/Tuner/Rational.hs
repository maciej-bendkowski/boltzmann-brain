{-|
 Module      : Data.Boltzmann.System.Tuner.Rational
 Description : Interface utilities with the Paganini tuner.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Tuner.Rational
    ( writeSpecification
    , paramSystem
    , toPSpec
    ) where

import System.IO

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Numeric.LinearAlgebra hiding (size)

import Data.Boltzmann.System

import Data.Boltzmann.Internal.Utils
import Data.Boltzmann.Internal.Tuner

toPSpec :: System Int -> PSpec
toPSpec sys = PSpec { numFreqs    = s
                    , numTypes    = t
                    , numSeqTypes = 0
                    }

   where t     = size sys
         alph  = alphabet sys
         s     = S.size (tunedLetters alph)

frequencies :: Set Letter -> [Double]
frequencies alph = mapMaybe freq (S.toList alph)

tunedLetters :: Set Letter -> Set Letter
tunedLetters = S.filter (isJust . freq)

writeSpecification :: System Int -> Handle -> IO ()
writeSpecification sys hout = do
    let spec = toPSpec sys

    -- # of equations and tuned symbols.
    writeListLn hout [numTypes spec, numFreqs spec]

    -- # requested letter frequencies.
    writeListLn hout (frequencies $ alphabet sys)

    -- type specifications
    let types' = types sys
    let alph   = tunedLetters $ alphabet sys
    mapM_ (typeSpecification hout alph types') (M.elems $ defs sys)

typeSpecification :: Handle -> Set Letter -> Set String -> [Cons Int] -> IO ()
typeSpecification hout alph types' cons = do
    let n = length cons
    hPrint hout n -- # of constructors
    mapM_ (consSpecification hout alph types') cons

sparsePrepend :: (Num a, Ord a)
              => (a, b) -> [(a, b)]
sparsePrepend x
  | fst x > 0 = [x]
  | otherwise = []

consSpecification :: Handle -> Set Letter -> Set String -> Cons Int -> IO ()
consSpecification hout alph types' con = do
    let z  = weight con
    let us = sparseMarkLetter alph z (func con) -- note: func con is the transition letter.
    let offset = 1 + S.size alph
    let ts = if isAtomic con then [] -- note: epsilon transition.
                             else sparseMarkType offset types' (head $ args con)

    writeListLn hout (sparsePrepend (z,0) ++ us ++ ts)

sparseMarkLetter :: Set Letter -> Int -> String -> [(Int,Int)]
sparseMarkLetter _ _ "_" = []
sparseMarkLetter alph w u =
    case letter `S.lookupIndex` alph of
      Nothing  -> []
      Just idx -> sparsePrepend (w, 1 + idx) -- note: offset for z
    where
        letter = Letter { symb = u, freq = Nothing, weightL = 0 }

sparseMarkType :: Int -> Set String -> Arg -> [(Int,Int)]
sparseMarkType offset types' (Type typ) = [(1, offset + idx)]
    where idx = typ `S.findIndex` types'

sparseMarkType _ _ _ = error "I wasn't expecting the Spanish inquisition!"

freqL :: Cons Int -> [Double] -> Alphabet -> Double
freqL con us alph =
    let f = func con -- note: func con is in fact the letter name.
        idx = Letter { symb = f, freq = Nothing, weightL = 0 } `S.findIndex` tunedLetters alph
        in case f `letterFreq` alph of
            Just _  -> us !! idx ^^ weight con
            Nothing -> 1

paramCons :: System Int -> Double -> Vector Double
          -> [Double] -> Double -> Cons Int -> Cons Double

paramCons sys rho ts us typW con
    | isAtomic con = con { weight = recip typW } -- note: epsilon transition
    | otherwise =
        let z    = rho ^^ weight con
            u    = freqL con us (alphabet sys)
            w    = z * u * value (argName $ head $ args con) sys ts
            in con { weight = w / typW }

paramType :: System Int -> Double -> Vector Double
          -> [Double] -> (String, [Cons Int]) -> (String, [Cons Double])

paramType sys rho ts us (t, cons) =
    (t, map (paramCons sys rho ts us typW) cons)
    where typW = value t sys ts

paramSystem :: System Int -> Double -> Vector Double
            -> [Double] -> PSystem Double

paramSystem sys rho ts us = sys'
    where typs   = M.toList (defs sys)
          types' = map (paramType sys rho ts us) typs
          sys'   = PSystem { system  = sys { defs = M.fromList types' }
                           , values  = ts
                           , param   = rho
                           , weights = sys
                           }
