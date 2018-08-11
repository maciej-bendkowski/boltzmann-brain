{-|
 Module      : Data.Boltzmann.System.Parser.Rational
 Description : Parser utilities for rational systems.
 Copyright   : (c) Maciej Bendkowski, 2017-2018

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Parser utilities meant to deal with rational system specifications.
 -}
module Data.Boltzmann.System.Parser.Rational
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

-- | Rational system specification parser.
systemStmt :: Parser (S.System Int)
systemStmt = sc *> systemStmt' <* eof
    where systemStmt' = do
            an   <- annotationParser
            alph <- alphabetStmt
            ds   <- some defsStmt
            return S.System { S.defs        = M.fromList ds
                            , S.annotations = M.fromList an
                            , S.alphabet    = alph
                            }

-- | Letter identifier.
letterIdent :: Parser String
letterIdent = lexeme $ some (alphaNumChar <|> char '_')

-- | Letter parser.
letterStmt :: Parser S.Letter
letterStmt = do
    letter  <- letterIdent
    letterF <- option (-1.0) (symbol ":" >> double)
    return S.Letter { S.symb = letter
                    , S.freq = toFreq letterF
                    }

-- | Alphabet specification parser.
alphabetStmt :: Parser S.Alphabet
alphabetStmt = do
    ls <- setBrackets $ letterStmt `sepBy1` symbol ","
    return $ Z.fromList ls

defsStmt :: Parser (String, [S.Cons Int])
defsStmt = do
    t <- identifier
    void (symbol "->")
    exprs <- exprListStmt
    return (t, exprs)

exprListStmt :: Parser [S.Cons Int]
exprListStmt = try (epsStmt <|> exprStmt) `sepBy1` symbol "|"

epsStmt :: Parser (S.Cons Int)
epsStmt = do
    void (symbol "_")
    return S.Cons { S.func      = "_"
                  , S.args      = []
                  , S.weight    = 0
                  , S.frequency = Nothing
                  }

exprStmt :: Parser (S.Cons Int)
exprStmt = do
    typ     <- identifier
    letter  <- parens letterIdent
    return S.Cons { S.func      = letter
                  , S.args      = [S.Type typ]
                  , S.weight    = length letter
                  , S.frequency = Nothing
                  }
