-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module System
    ( System(..)
    , size 
    , Cons(..)
    , Arg(..)
    , SystemError
    , SystemMonad
    , reportSystemError
    , consistent
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

instance Show SystemError where
    show (Inconsistent t con arg) = "Invalid argument type '" 
        ++ arg ++ "' in constructor " ++ con ++ " of type " ++ t ++ "."

reportSystemError :: SystemError -> IO ()
reportSystemError = print

type SystemMonad = Either SystemError

consistent :: System a -> SystemMonad ()
consistent sys = mapM_ consistentType (M.toList $ defs sys) `catchError` Left
    where ts = types sys
          consistentType (t,cons) = mapM_ (consistentCons t) cons
          consistentCons t con = mapM_ (consistentArg t con) $ args con
          
          consistentArg :: String -> Cons a -> Arg -> SystemMonad ()
          consistentArg t con (Type s)
            | s `S.member` ts = return () 
            | otherwise = throwError $ Inconsistent t (func con) s
