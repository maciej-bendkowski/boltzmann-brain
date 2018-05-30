{-|
 Module      : Data.Boltzmann.System.Parser
 Description : Parser utilities for combinatorial systems.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Parser
    ( parseSystem
    ) where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Data.Map.Strict as M

import Data.Boltzmann.Internal.Parser
import qualified Data.Boltzmann.System as S

-- | Identifier producer.
identifierP :: Parser Char
            -> (Parser Char -> Parser String)
            -> Parser String

identifierP p f = lexeme $ (:) <$> p <*> f (alphaNumChar <|> char '_')

identifier :: Parser String
identifier = identifierP upperChar many

toFreq :: Double -> Maybe Double
toFreq x
    | x < 0     = Nothing
    | otherwise = Just x

systemStmt :: Parser (S.System Int)
systemStmt = sc *> systemStmt' <* eof
    where systemStmt' = do
            an <- many annotationStmt
            ds <- some defsStmt
            return S.System { S.defs        = M.fromList ds
                            , S.annotations = M.fromList an
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

annotationIdentifier :: Parser String
annotationIdentifier = identifierP (char '@') some

annotationValue :: Parser String
annotationValue = lexeme $ some (alphaNumChar <|> punctuationChar)

-- | System annotations.
annotationStmt :: Parser (String, String)
annotationStmt = do
    lhs <- annotationIdentifier
    rhs <- annotationValue
    return (tail lhs, rhs)

-- | Parses the given system specification.
parseSystem :: String
            -> IO (Either (ParseError Char Dec) (S.System Int))

parseSystem = parseFromFile systemStmt
