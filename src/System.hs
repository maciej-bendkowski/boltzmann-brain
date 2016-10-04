-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module System
    ( System(..)
    , size 
    , Cons(..)
    , Arg(..)
    , SystemError
    , SystemMonad
    , errors
    ) where

import Control.Monad.Except
import qualified Data.Map.Strict as M

import Data.Set (Set)
import qualified Data.Set as S

data System a = System { defs :: M.Map String [Cons a] 
                       } deriving (Show)

size :: System a -> Int
size = M.size . defs

data Cons a = Cons { func :: String
                   , args :: [Arg]
                   , weight :: a
                   } deriving (Show)

data Arg = Type String
           deriving (Show)

types :: System a -> Set String
types = M.keysSet . defs

data SystemError = Inconsistent String -- type name 
                                String -- constructor name
                                String -- argument name
                 
                 | InvalidCons  String -- type name
                                String -- constructor name

instance Show SystemError where
    show (Inconsistent t con arg) = "Invalid argument type '" 
        ++ arg ++ "' in constructor " ++ con ++ " of type " ++ t ++ "."
    
    show (InvalidCons t con) = "Invalid constructor '" ++ con 
        ++ "' in type " ++ t ++ " - '" ++ con ++ "' names a declared type."

type SystemMonad = Either SystemError

errors :: System a -> SystemMonad ()
errors sys = do
    void $ consistent sys
    void $ validCons sys

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
