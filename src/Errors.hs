-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Errors
    ( SystemError
    , SystemMonad
    , errors
    ) where

import Control.Monad.Except
import qualified Data.Map.Strict as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import System

data SystemError = Inconsistent String -- type name 
                                String -- constructor name
                                String -- argument name
                 
                 | InvalidCons  String -- type name
                                String -- constructor name
                 
                 | ClashCons    [String] -- clashing constructors

instance Show SystemError where
    show (Inconsistent t con arg) = "[Error] Invalid argument type '" 
        ++ arg ++ "' in constructor " ++ con ++ " of type " ++ t ++ "."
    
    show (InvalidCons t con) = "[Error] Invalid constructor '" ++ con 
        ++ "' in type " ++ t ++ ": '" ++ con ++ "' names a declared type."

    show (ClashCons cons) = "[Error] Clashing constructor names: "
        ++ foldl1 (\c c' -> "'" ++ c ++ "', " ++ "'" ++ c' ++ "'") cons
        ++ "."

type SystemMonad = Either SystemError

errors :: System a -> SystemMonad ()
errors sys = do
    void $ consistent sys
    void $ validCons sys
    void $ clashCons sys

consistent :: System a -> SystemMonad ()
consistent sys = mapM_ consistentType (M.toList $ defs sys) `catchError` Left
    where ts = types sys
          consistentType (t,cons) = mapM_ (consistentCons t) cons
          consistentCons t con = mapM_ (consistentArg t con) $ args con
          
          consistentArg :: String -> Cons a -> Arg -> SystemMonad ()
          consistentArg t con (Type s)
            | s `S.member` ts = return () 
            | otherwise = throwError $ Inconsistent t (func con) s

validCons :: System a -> SystemMonad ()
validCons sys = mapM_ validType (M.toList $ defs sys) `catchError` Left
    where ts = types sys
          validType (t,cons) = mapM_ (validCon t) cons
          
          validCon :: String -> Cons a -> SystemMonad ()
          validCon t con
            | null (args con) && func con `S.member` ts = 
                throwError $ InvalidCons t (func con)
            | otherwise = return ()

consNames :: System a -> MultiSet String
consNames sys = MultiSet.unions (map insT $ M.elems (defs sys))
    where insT = MultiSet.fromList . map func

duplicates :: System a -> [String]
duplicates sys = map fst $ filter gather $ MultiSet.toOccurList ms
    where gather (con,n) = n /= 1
          ms = consNames sys

clashCons :: System a -> SystemMonad ()
clashCons sys = let cs = duplicates sys in
                    unless (null cs) $ throwError (ClashCons cs) `catchError` Left
