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
    , printError
    ) where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L

import qualified Data.Map.Strict as M

import qualified Data.Boltzmann.System as S

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt  = L.skipLineComment "--"
          blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

integer :: Parser Int
integer = lexeme $ do
    n <- L.integer
    return $ fromIntegral n

identifier :: Parser String
identifier = lexeme $ (:) <$> upperChar <*> many (alphaNumChar <|> char '_')

systemStmt :: Parser (S.System Int)
systemStmt = sc *> systemStmt' <* eof
    where systemStmt' = do
            ds <- some defsStmt
            return S.System { S.defs = M.fromList ds } 

defsStmt :: Parser (String, [S.Cons Int])
defsStmt = do
    t <- identifier
    void (symbol "=")
    exprs <- exprListStmt
    return (t, exprs)

exprListStmt :: Parser [S.Cons Int]
exprListStmt = do
    stms <- exprStmt `sepBy` symbol "|"
    void (symbol ".")
    return stms

exprStmt :: Parser (S.Cons Int)
exprStmt = do
    f <- identifier
    as <- many argStmt
    w <- option 1 (parens integer)
    return S.Cons { S.func = f
                  , S.args = as
                  , S.weight = w
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

parseFromFile :: Parsec e String a 
              -> String 
              -> IO (Either (ParseError Char e) a)

parseFromFile p file = runParser p file <$> readFile file

-- | Parses the given system specification.
parseSystem :: String 
            -> IO (Either (ParseError Char Dec) (S.System Int))

parseSystem = parseFromFile systemStmt

-- | Prints the given parsing errors.
printError :: ParseError Char Dec -> IO ()
printError err = putStr $ parseErrorPretty err
