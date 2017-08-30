{-|
 Module      : Data.Boltzmann.System.Errors
 Description : Various error handling utilities.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Errors
    ( SystemError
    , ErrorMonad
    , errors
    ) where

import Control.Monad.Except

import Data.Map (Map)
import qualified Data.Map.Strict as M

import qualified Data.Set as S

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import Data.Char (isUpper)
import Text.Read (readMaybe)

import Data.Boltzmann.System
import Data.Boltzmann.System.Jacobian

-- | Semantic system errors referring to invalid
--   input data, for instance ill-founded systems.
data SystemError = Inconsistent String                -- type name
                                String                -- constructor name
                                String                -- argument name

                 | InvalidCons  String                -- type name
                                String                -- constructor name

                 | NullCons     String                -- type name
                                String                -- constructor name

                 | ClashCons    [String]              -- clashing constructors
                 | Illfounded                         -- ill-founded system
                 | Infinite                           -- infinite structures

                 | Frequencies  [String]              -- incorrect frequencies

                 | InvalidPrecision                   -- invalid precision
                 | InvalidMaxIter                     -- invalid maxiter
                 | InvalidModule                      -- invalid module

                 | UnsupportedSystemType String       -- invalid system type

instance Show SystemError where
    show (Inconsistent t con arg) = "[Error] Invalid argument type '"
        ++ arg ++ "' in constructor " ++ con ++ " of type " ++ t ++ "."

    show (InvalidCons t con) = "[Error] Invalid constructor '" ++ con
        ++ "' in type " ++ t ++ ": '" ++ con ++ "' names a declared type."

    show (NullCons t con) = "[Error] Invalid constructor '" ++ con
        ++ "' in type " ++ t ++ ": encountered a structure of size 0."

    show (ClashCons cons) = "[Error] Clashing constructor names: "
        ++ foldl1 (\c c' -> "'" ++ c ++ "', " ++ "'" ++ c' ++ "'") cons
        ++ "."

    show Illfounded = "[Error] Ill-founded system."

    show Infinite = "[Error] System defines no finite structures."

    show (Frequencies ts) = "[Error] Incorrect frequencies (expected real in [0.0,1.0]): "
        ++ foldl1 (\c c' -> "'" ++ c ++ "', " ++ "'" ++ c' ++ "'") ts
        ++ "."

    show InvalidPrecision = "[Error] Invalid precision annotation. "
            ++ "Expected a positive floating point number."

    show InvalidMaxIter = "[Error] Invalid maxiter annotation. "
            ++ "Expected a positive integer."

    show InvalidModule = "[Error] Invalid module annotation. "
            ++ "Expected a name starting with an upper case letter."

    show (UnsupportedSystemType s) = "[Error] Unsupported system type. " ++ s

-- | Monadic error handling wrapper.
type ErrorMonad = Either SystemError

-- | Checks whether the given input system is correct yielding its type.
--   Otherwise, returns an appropriate SystemError.
errors :: Bool -> System Int -> ErrorMonad SystemType
errors useForce sys = do
    void $ consistent sys
    void $ validCons sys
    void $ clashCons sys
    void $ nullCons sys
    void $ infinite sys
    void $ incorrectFrequencies sys
    void $ invalidAnnotations sys
    unless useForce $ illfounded sys
    invalidSystemType sys

invalidSystemType :: System a -> ErrorMonad SystemType
invalidSystemType sys =
    case systemType sys of
      (Unsupported s) -> throwError (UnsupportedSystemType s) `catchError` Left
      sysT            -> return sysT

infinite :: System a -> ErrorMonad ()
infinite sys = unless (hasAtoms sys || not (null $ seqTypes sys)) $ throwError Infinite `catchError` Left

consistent :: System a -> ErrorMonad ()
consistent sys = mapM_ consistentType (M.toList $ defs sys) `catchError` Left
    where ts = types sys
          consistentType (t,cons) = mapM_ (consistentCons t) cons
          consistentCons t con    = mapM_ (consistentArg t con) $ args con

          consistentArg :: String -> Cons a -> Arg -> ErrorMonad ()
          consistentArg t con (List s)
            | s `S.member` ts = return ()
            | otherwise = throwError $ Inconsistent t (func con) s
          consistentArg t con (Type s)
            | s `S.member` ts = return ()
            | otherwise = throwError $ Inconsistent t (func con) s

validCons :: System a -> ErrorMonad ()
validCons sys = mapM_ validType (M.toList $ defs sys) `catchError` Left
    where ts = types sys
          validType (t,cons) = mapM_ (validCon t) cons

          validCon :: String -> Cons a -> ErrorMonad ()
          validCon t con
            | null (args con) && func con `S.member` ts =
                throwError $ InvalidCons t (func con)
            | otherwise = return ()

nullCons :: (Num a, Eq a) => System a -> ErrorMonad ()
nullCons sys = mapM_ nullType (M.toList $ defs sys) `catchError` Left
    where nullType (t,cons) = mapM_ (nullCon t) cons

          nullCon :: (Num a, Eq a) => String -> Cons a -> ErrorMonad ()
          nullCon t con
            | null (args con) && weight con == 0 =
                throwError $ NullCons t (func con)
            | otherwise = return ()

consNames :: System a -> MultiSet String
consNames sys = MultiSet.unions (map insT $ M.elems (defs sys))
    where insT = MultiSet.fromList . map func

duplicates :: System a -> [String]
duplicates sys = map fst $ filter gather $ MultiSet.toOccurList ms
    where gather (_,n) = n /= 1
          ms           = consNames sys

clashCons :: System a -> ErrorMonad ()
clashCons sys = let cs = duplicates sys in
                    unless (null cs) $ throwError (ClashCons cs) `catchError` Left

illfounded :: System Int -> ErrorMonad ()
illfounded sys = unless (wellFounded sys) $ throwError Illfounded `catchError` Left

incorrectFrequencies :: System Int -> ErrorMonad ()
incorrectFrequencies sys = unless (null fs) $ throwError (Frequencies fs) `catchError` Left
    where fs = incorrectFrequencies' sys

incorrectFrequencies' :: System Int -> [String]
incorrectFrequencies' sys = concatMap incF $ M.elems (defs sys)
    where incF cons  = map func $ filter incF' cons
          incF' cons = case frequency cons of
                         Nothing -> False
                         Just f  -> 0.0 > f || 1.0 < f

-- | General, compiler-independent admissible annotations.
invalidAnnotations :: System Int -> ErrorMonad ()
invalidAnnotations sys = do
    let ann = annotations sys
    void $ precisionAnnotation ann
    void $ maxiterAnnotation ann
    moduleAnnotation ann

precisionAnnotation :: Map String String -> ErrorMonad ()
precisionAnnotation ann =
    case "precision" `M.lookup` ann of
      Nothing -> return ()
      Just x -> case readMaybe x :: Maybe Double of
                  Nothing -> throwError InvalidPrecision
                  Just x' -> unless (x' > 0) $ throwError InvalidPrecision `catchError` Left

maxiterAnnotation :: Map String String -> ErrorMonad ()
maxiterAnnotation ann =
    case "maxiter" `M.lookup` ann of
      Nothing -> return ()
      Just x -> case readMaybe x :: Maybe Int of
                  Nothing -> throwError InvalidMaxIter
                  Just x' -> unless (x' > 0) $ throwError InvalidMaxIter `catchError` Left

moduleAnnotation :: Map String String -> ErrorMonad ()
moduleAnnotation ann =
    case "module" `M.lookup` ann of
      Nothing -> return ()
      Just x -> unless (isUpper $ head x) $ throwError InvalidModule `catchError` Left
