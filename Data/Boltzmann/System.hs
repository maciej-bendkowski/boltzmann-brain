{-|
 Module      : Data.Boltzmann.System
 Description : System utilities for combinatorial specifications.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 General utilities for combinatorial system of algebraic and rational systems.
 -}
{-# LANGUAGE OverloadedStrings #-}
module Data.Boltzmann.System
    ( Alphabet
    , Letter(..)
    , Format(..)
    , isAlgebraicF
    , isRationalF
    , letterFreq
    , letterWeight
    , lookupLetter

    , System(..)
    , size
    , constructors
    , Cons(..)
    , Arg(..)
    , argName
    , types

    , PSystem(..)
    , typeList
    , initType
    , paramTypes
    , paramTypesW
    , typeWeight
    , seqTypes

    , SystemType(..)
    , systemType
    , hasAtoms
    , isAtomic

    , evalT
    , evalC
    , evalA
    , getIdx
    , value
    , eval

    , SystemT
    , toSystemT
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map.Strict as M

import Numeric.LinearAlgebra hiding (size)

import Data.Maybe (mapMaybe)

import Data.List (nub)
import Data.Graph

import Data.Aeson

import Data.Boltzmann.System.Annotations

-- | Set of letters with auxiliary frequencies.
--   Note: Used for rational specification only.
type Alphabet = Set Letter

-- | Letter symbols with optional frequencies.
data Letter = Letter { symb    :: String
                     , freq    :: Maybe Double
                     , weightL :: Int
                     } deriving (Show,Eq)

-- | Given a string and an alphabet, finds
--   the corresponding letter frequency.
letterFreq :: String -> Alphabet -> Maybe Double
letterFreq s alph =
    let x = Letter { symb = s, freq = Nothing, weightL = 0 }
        in case x `S.lookupIndex` alph of
            Nothing -> Nothing
            Just idx -> freq (idx `S.elemAt` alph)

-- | Given a string and an alphabet, finds
--   the corresponding letter weight.
letterWeight :: String -> Alphabet -> Maybe Int
letterWeight s alph =
    let x = Letter { symb = s, freq = Nothing, weightL = 0 }
        in case x `S.lookupIndex` alph of
            Nothing -> Nothing
            Just idx -> Just $ weightL (idx `S.elemAt` alph)

-- | Given a string and an alphabet, finds
--   the corresponding letter index.
lookupLetter :: String -> Alphabet -> Maybe Int
lookupLetter s alph =
    let x = Letter { symb = s, freq = Nothing, weightL = 0 }
      in x `S.lookupIndex` alph

instance Ord Letter where
    compare a b = compare (symb a) (symb b) -- ignore frequencies.

instance ToJSON Letter where
    toJSON letter = object ["symbol" .= symb letter
                           ,"freq"   .= freq letter
                           ,"weight" .= weightL letter
                           ]

-- | Input specification format.
data Format = AlgebraicF
            | RationalF

instance Show Format where
    show AlgebraicF = "algebraic"
    show RationalF  = "rational"

isAlgebraicF :: Format -> Bool
isAlgebraicF AlgebraicF = True
isAlgebraicF _          = False

isRationalF :: Format -> Bool
isRationalF = not . isAlgebraicF

-- | System of combinatorial structures.
data System a = System { defs        :: Map String [Cons a] -- ^ Type definitions.
                       , alphabet    :: Alphabet            -- ^ System alphabet.
                       , annotations :: Annotations         -- ^ System annotations.
                       } deriving (Show)

data SystemT a = SystemT { systemTypes       :: [TypeT a]
                         , systemAlphabet    :: Alphabet
                         , systemAnnotations :: Annotations
                         } deriving (Show)

instance ToJSON a => ToJSON (SystemT a) where
        toJSON sys = object ["types"       .= systemTypes sys
                            ,"alphabet"    .= systemAlphabet sys
                            ,"annotations" .= systemAnnotations sys]

data TypeT a = TypeT { typeName :: String
                     , constrs  :: [ConsT a]
                     } deriving (Show)

instance ToJSON a => ToJSON (TypeT a) where
        toJSON t = object ["name"         .= typeName t
                          ,"constructors" .= constrs t
                          ]

data ConsT a = ConsT { constrName   :: String
                     , arguments    :: [ArgT]
                     , constrWeight :: a
                     , constrFreq   :: Maybe Double
                     } deriving (Show)

instance ToJSON a => ToJSON (ConsT a) where
        toJSON t = object ["name"   .= constrName t
                          ,"args"   .= arguments t
                          ,"weight" .= constrWeight t
                          ,"freq"   .= constrFreq t
                          ]

data ArgT = ArgT { argumentName :: String
                 , argumentType :: String
                 } deriving (Show)

instance ToJSON ArgT where
        toJSON t = object ["name" .= argumentName t
                          ,"type" .= argumentType t
                          ]

-- | Converts a given system to an output format.
toSystemT :: System a -> SystemT a
toSystemT sys =
        SystemT { systemTypes       = map toTypeT (M.toList $ defs sys)
                , systemAlphabet    = alphabet sys
                , systemAnnotations = annotations sys
                }

toTypeT :: (String, [Cons a]) -> TypeT a
toTypeT (t, cons) = TypeT { typeName = t
                          , constrs  = map toConsT cons
                          }

toConsT :: Cons a -> ConsT a
toConsT con = ConsT { constrName   = func con
                    , arguments    = map toArgT (args con)
                    , constrWeight = weight con
                    , constrFreq   = frequency con
                    }

toArgT :: Arg -> ArgT
toArgT (Type arg) = ArgT { argumentName = arg, argumentType = "type" }
toArgT (List arg) = ArgT { argumentName = arg, argumentType = "list" }

-- | Size of a combinatorial system.
size :: System a -> Int
size = M.size . defs

-- | First (in alphabetical order) type in system.
initType :: System a -> String
initType = head . M.keys . defs

-- | Constructors of a combinatorial system.
constructors :: System a -> Int
constructors = length . concat . M.elems . defs

-- | Type constructor.
data Cons a = Cons { func      :: String        -- ^ Constructor name.
                   , args      :: [Arg]         -- ^ Argument list.
                   , weight    :: a             -- ^ Constructor weight.
                   , frequency :: Maybe Double  -- ^ Marking parameter.
                   } deriving (Eq,Show)

-- | Type constructor arguments.
data Arg = Type String                       -- ^ Regular type reference.
         | List String                       -- ^ Type list reference.
           deriving (Eq,Show)

-- | The name of an argument.
argName :: Arg -> String
argName (Type s) = s
argName (List s) = s

-- | Type set of the given system.
types :: System a -> Set String
types = M.keysSet . defs

-- | Parametrised system of combinatorial structures.
data PSystem a = PSystem { system  :: System a      -- ^ System with probability weights.
                         , values  :: Vector a      -- ^ Numerical values of corresponding types.
                         , param   :: a             -- ^ Evaluation parameter.
                         , weights :: System Int    -- ^ System with input weights.
                         }

-- | Type list of the given parametrised system.
typeList :: PSystem a -> [String]
typeList = S.toList . M.keysSet . defs . system

-- | List of types with corresponding constructors.
paramTypes :: PSystem a -> [(String, [Cons a])]
paramTypes = M.toList . defs . system

-- | List of types with corresponding constructors and input weights.
paramTypesW :: PSystem a -> [(String, [(Cons a, Int)])]
paramTypesW sys = map (addW $ weights sys) xs
    where xs = paramTypes sys

addW :: System Int -> (String, [a]) -> (String, [(a, Int)])
addW sys (s, cons) = (s, zip cons ws)
    where ws = typeW sys s

typeW :: System Int -> String -> [Int]
typeW sys s = case s `M.lookup` defs sys of
    Just cons -> map weight cons
    Nothing -> []

-- | Type weight of the given parametrised system.
typeWeight :: PSystem Double -> String -> Double
typeWeight sys t = vec ! idx
    where m   = defs $ system sys
          vec = values sys
          idx = M.findIndex t m

-- | List of sequence types.
seqTypes :: System a -> [String]
seqTypes = S.elems . S.fromList . concatMap seqTypesCons
            . concat . M.elems . defs

seqTypesCons :: Cons a -> [String]
seqTypesCons = mapMaybe listN . args
    where listN (List s) = Just s
          listN _        = Nothing

-- | Checks it the argument is a list.
isListArg :: Arg -> Bool
isListArg (List _) = True
isListArg  _       = False

-- | Type of a combinatorial system.
--   Note: System other than rational or algebraic are not yet supported.
data SystemType = Rational
                | Algebraic
                | Unsupported String   -- ^ error message

instance Show SystemType where
    show Rational        = "rational"
    show Algebraic       = "algebraic"
    show (Unsupported _) = "unsupported"

-- | Determines the system type.
systemType :: System a -> SystemType
systemType sys
  | not (isLinear sys)        = Algebraic
  | not (isInterruptible sys) = Unsupported "Given rational system is not interruptible."
  | otherwise =
    let depGraph = dependencyGraph sys
     in case scc depGraph of
          [_] -> Rational
          xs  -> Unsupported $ "Given rational system has "
                    ++ show (length xs) ++ " strongly connected components."

-- | Constructs a dependency graph for the given system.
dependencyGraph :: System a -> Graph
dependencyGraph sys = buildG (0,n+d-1) (edgs ++ edgs')
    where idx s      = M.findIndex s (defs sys)
          idxSeq s   = n + S.findIndex s seqsSet
          edgs       = concatMap (edges' atomicT idx idxSeq) $ M.toList (defs sys)
          edgs'      = concatMap (\t -> [(idxSeq t, idxSeq t),
                                    (idxSeq t, idx t)]) seqs
          atomicT    = atomicTypes sys
          seqsSet    = S.fromAscList seqs
          seqs       = seqTypes sys
          d          = S.size seqsSet
          n          = size sys

edges' :: Set String -> (String -> Int) -> (String -> Int) -> (String, [Cons b]) -> [(Vertex, Vertex)]
edges' atomicT idx idxSeq (t,cons) = concatMap edge' $ neighbours cons
    where tidx           = idx t
          neighbours     = nub . concatMap args
          edge' (List s) = [(tidx, idxSeq s)]
          edge' (Type s)
            | s `S.member` atomicT = [(tidx, idx s), (idx s, tidx)] -- double edge
            | otherwise = [(tidx, idx s)]

-- | Checks whether the system is linear, i.e.
--   each constructor references at most one type.
isLinear :: System a -> Bool
isLinear sys = all (all linear) (M.elems $ defs sys)
    where atomicT     = atomicTypes sys
          linear cons =  not (any isListArg $ args cons)
            && length (compoundArgs atomicT $ args cons) <= 1

-- | Determines whether each constructor in the system has at most one atom.
--   Note: the system is assumed to contain some atoms (see hasAtoms).
isInterruptible :: System a -> Bool
isInterruptible sys = all interruptible' $ M.elems (defs sys)
    where interruptible' cons = length (filter isAtomic cons) <= 1

compoundArgs :: Set String -> [Arg] -> [Arg]
compoundArgs atomicT = filter (\x -> argName x `S.notMember` atomicT)

-- | Determines the set of "atomic" types.
atomicTypes :: System a -> Set String
atomicTypes sys = S.fromList $ map fst ts
    where ts = filter isAtomic' $ M.toList (defs sys)
          isAtomic' (_,cons) = all isAtomic cons

isAtomic :: Cons a -> Bool
isAtomic = null . args

-- | Determines whether the system has atoms.
hasAtoms :: System a -> Bool
hasAtoms sys = any (any isAtomic) $ M.elems (defs sys)

-- | Evaluates the type in the given coordinates.
evalT :: System Int -> Double -> Vector Double -> [Cons Int] -> Double
evalT sys z ys cons = sum $ map (evalC sys z ys) cons

-- | Evaluates the constructor in the given coordinates.
evalC :: System Int -> Double -> Vector Double -> Cons Int -> Double
evalC sys z ys con = foldl (*) start $ map (evalA sys ys) (args con)
    where w = weight con
          start = if w > 0 then z ^^ w
                           else 1

-- | Evaluates the argument in the given coordinates.
evalA :: System Int -> Vector Double -> Arg -> Double
evalA sys ys (Type t) = ys ! getIdx sys t
evalA sys ys (List t) = recip $ 1 - ys ! getIdx sys t

getIdx :: System Int -> String -> Int
getIdx sys x = x `M.findIndex` defs sys

value :: String -> System b -> Vector Double -> Double
value t sys vec = vec ! M.findIndex t (defs sys)

-- | Evaluates the system at the given coordinates.
eval :: System Int -> Vector Double -> Double -> Vector Double
eval sys ys z = n |> map update [0..n]
    where n = size sys
          f k = snd $ M.elemAt k (defs sys)
          update idx = evalT sys z ys $ f idx
