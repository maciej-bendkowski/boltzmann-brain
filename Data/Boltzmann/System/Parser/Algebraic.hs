{-|
 Module      : Data.Boltzmann.System.Parser.Algebraic
 Description : Parser utilities for algebraic specifications.
 Copyright   : (c) Maciej Bendkowski, 2017-2019

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Parser utilities meant to deal with algebraic system specifications.
 -}
module Data.Boltzmann.System.Parser.Algebraic
    ( systemStmt
    ) where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Set as Z
import qualified Data.Map.Strict as M

import Data.Boltzmann.Internal.Parser
import qualified Data.Boltzmann.System as S

import Data.Boltzmann.System.Annotations

-- | Algebraic system specification parser.
systemStmt :: Parser (S.System Int)
systemStmt = sc *> systemStmt' <* eof
    where systemStmt' = do
            an <- annotationParser
            ds <- some defsStmt
            return S.System { S.defs        = M.fromList ds
                            , S.annotations = M.fromList an
                            , S.alphabet    = Z.empty
                            }

defsStmt :: Parser (String, [S.Cons Int])
defsStmt = do
    t <- identifier
    void (symbol "=")
    exprs <- try (abbrevdef t) <|> exprListStmt
    return (t, exprs)

abbrevdef :: String -> Parser [S.Cons Int]
abbrevdef f = try (listdef f) <|> tupledef f

listdef :: String -> Parser [S.Cons Int]
listdef f = do
    xs <- listStmt
    m  <- option (-1.0) (brackets double)
    void (symbol ".")
    return [S.Cons { S.func      = f
                   , S.args      = [xs]
                   , S.weight    = 0
                   , S.frequency = toFreq m
                   }]

tupledef :: String -> Parser [S.Cons Int]
tupledef f = do
    ids <- parens $ identifier `sepBy2` symbol ","
    m  <- option (-1.0) (brackets double)
    void (symbol ".")
    return [S.Cons { S.func      = f
                   , S.args      = map S.Type ids
                   , S.weight    = 0
                   , S.frequency = toFreq m
                   }]

exprListStmt :: Parser [S.Cons Int]
exprListStmt = do
    stms <- exprStmt `sepBy1` symbol "|"
    void (symbol ".")
    return stms

exprStmt :: Parser (S.Cons Int)
exprStmt = do
    f  <- identifier
    as <- many argStmt
    w  <- option 1 (parens integer)
    m  <- option (-1.0) (brackets double)
    return S.Cons { S.func      = f
                  , S.args      = as
                  , S.weight    = w
                  , S.frequency = toFreq m
                  }

argStmt :: Parser S.Arg
argStmt = try listStmt <|> typeStmt

listStmt :: Parser S.Arg
listStmt = do
    t <- brackets identifier
    return $ S.List t

typeStmt :: Parser S.Arg
typeStmt = do
    t <- identifier
    return $ S.Type t
