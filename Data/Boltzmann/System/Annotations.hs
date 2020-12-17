{-|
 Module      : Data.Boltzmann.System.Annotations
 Description : Annotation utilities for combinatorial samplers.
 Copyright   : (c) Maciej Bendkowski, 2017-2020

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.System.Annotations
    ( Annotations
    , annotationParser
    ) where

import Data.Map (Map)

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

import Data.Boltzmann.Internal.Parser

type Parser = Parsec Void String

annIdentifier :: Parser String
annIdentifier = identifierP (char '@') some

annValue :: Parser String
annValue = lexeme $ some (alphaNumChar <|> punctuationChar)

-- | System annotations.
annotationStmt :: Parser (String, String)
annotationStmt = do
    lhs <- annIdentifier
    rhs <- annValue
    return (tail lhs, rhs) -- note: dropping the leading '@'

annotationParser :: Parser [(String, String)]
annotationParser = many annotationStmt

-- | System annotations.
type Annotations = Map String String
