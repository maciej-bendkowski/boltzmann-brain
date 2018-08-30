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
            ds   <- some (defsStmt alph)
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
    letterW <- option (length letter) (parens integer)
    return S.Letter { S.symb    = letter
                    , S.freq    = toFreq letterF
                    , S.weightL = letterW
                    }

-- | Alphabet specification parser.
alphabetStmt :: Parser S.Alphabet
alphabetStmt = do
    ls <- setBrackets $ letterStmt `sepBy1` symbol ","
    return $ Z.fromList ls

defsStmt :: S.Alphabet -> Parser (String, [S.Cons Int])
defsStmt alph = do
    t <- identifier
    void (symbol "->")
    exprs <- exprListStmt alph
    return (t, exprs)

exprListStmt :: S.Alphabet -> Parser [S.Cons Int]
exprListStmt alph = try (epsStmt <|> exprStmt alph) `sepBy1` symbol "|"

epsStmt :: Parser (S.Cons Int)
epsStmt = do
    void (symbol "_")
    return S.Cons { S.func      = "_"
                  , S.args      = []
                  , S.weight    = 0
                  , S.frequency = Nothing
                  }

exprStmt :: S.Alphabet -> Parser (S.Cons Int)
exprStmt alph = do
    typ     <- identifier
    letter  <- option "" (parens letterIdent) -- potential epsilon transitions.
    let x   = S.Letter { S.symb = letter, S.freq = Nothing, S.weightL = 0 }
    let w = case x `Z.lookupIndex` alph of
                Nothing -> 0 -- epsilon transitions or not existing symbols.
                Just idx -> S.weightL $ Z.elemAt idx alph

    return S.Cons { S.func      = letter
                  , S.args      = [S.Type typ]
                  , S.frequency = Nothing
                  , S.weight    = w
                  }
