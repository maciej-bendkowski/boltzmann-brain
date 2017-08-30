{-|
 Module      : Data.Boltzmann.Internal.ParserUtils
 Description : Parser utilities for combinatorial systems.
 Copyright   : (c) Maciej Bendkowski, 2017

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental
 -}
module Data.Boltzmann.Internal.ParserUtils
    ( sc

    , lexeme
    , symbol
    , parens
    , brackets
    , integer
    , double

    , sepBy2
    , parseN

    , parseFromFile
    , printError
    ) where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

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

double :: Parser Double
double = lexeme L.float

sepBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 p q = do
    x <- p
    void q
    xs <- p `sepBy1` q
    return (x : xs)

-- | n-fold parser application.
parseN :: Parser a -> Int -> Parser [a]
parseN _ 0 = return []
parseN p n = do
    x <- p
    xs <- parseN p (n-1)
    return $ x : xs

-- | Uses an input file for parsing.
parseFromFile :: Parsec e String a
              -> String
              -> IO (Either (ParseError Char e) a)

parseFromFile p file = runParser p file <$> readFile file

-- | Prints the given parsing errors.
printError :: ParseError Char Dec -> IO ()
printError err = putStr $ parseErrorPretty err
