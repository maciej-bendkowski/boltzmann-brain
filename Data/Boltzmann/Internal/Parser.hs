{-|
 Module      : Data.Boltzmann.Internal.Parser
 Description : Parser utilities for combinatorial systems.
 Copyright   : (c) Maciej Bendkowski, 2017-2020

 License     : BSD3
 Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
 Stability   : experimental

 Common parser utilities and helper functions.
 -}
module Data.Boltzmann.Internal.Parser
  ( sc
  , identifierP
  , identifier
  , toFreq
  , lexeme
  , symbol
  , parens
  , brackets
  , setBrackets
  , integer
  , double
  , sepBy2
  , parseN
  , parseFromFile
  , printError
  )
where

import           Control.Monad                  ( void )
import           Data.Void
import           System.Exit
import           System.IO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void String

-- | Identifier producer.
identifierP :: Parser Char -> (Parser Char -> Parser String) -> Parser String
identifierP p f = lexeme $ (:) <$> p <*> f (alphaNumChar <|> char '_')

identifier :: Parser String
identifier = identifierP upperChar many

toFreq :: Int -> Maybe Int
toFreq x | x < 0     = Nothing
         | otherwise = Just x

-- | Cut-out block and line comments parser.
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "--"
  blockCmnt = L.skipBlockComment "{-" "-}"

-- | Lexeme parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parenthesis parser.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Brackets parser.
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Curly brackets parser.
setBrackets :: Parser a -> Parser a
setBrackets = between (symbol "{") (symbol "}")

-- | Integer parser.
integer :: Parser Int
integer = lexeme L.decimal

-- | Double parser.
double :: Parser Double
double = lexeme L.float

-- | Separate by two parser.
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
  x  <- p
  xs <- parseN p (n - 1)
  return $ x : xs

-- | Uses an input file for parsing.
parseFromFile
  :: Parsec e String a -> String -> IO (Either (ParseErrorBundle String e) a)

parseFromFile p file = runParser p file <$> readFile file

-- | Prints the given parsing errors.
printError :: (Stream t, ShowErrorComponent e) => ParseErrorBundle t e -> IO a

printError err = do
  hPutStr stderr $ errorBundlePretty err
  exitWith (ExitFailure 1)
