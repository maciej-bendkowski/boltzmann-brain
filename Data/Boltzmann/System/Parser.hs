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
    , parsePaganini
    , printError
    ) where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L

import qualified Data.Map.Strict as M

import qualified Data.Boltzmann.System as S
import qualified Data.Boltzmann.System.Paganini as P

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

identifier :: Parser String
identifier = lexeme $ (:) <$> upperChar <*> many (alphaNumChar <|> char '_')

toFreq :: Double -> Maybe Double
toFreq x
    | x < 0     = Nothing
    | otherwise = Just x

systemStmt :: Parser (S.System Int)
systemStmt = sc *> systemStmt' <* eof
    where systemStmt' = do
            ds <- some defsStmt
            return S.System { S.defs = M.fromList ds }

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

parseN :: Parser a -> Int -> Parser [a]
parseN _ 0 = return []
parseN p n = do
    x <- p
    xs <- parseN p (n-1)
    return $ x : xs

paganiniStmt :: P.PSpec -> Parser (Double, [Double], [Double])
paganiniStmt spec = do
    z <- double
    us <- parseN double $ P.numFreqs spec
    ts <- parseN double $ P.numTypes spec
    return (z,us,ts)

-- | Parses the given Paganini specification.
parsePaganini :: P.PSpec -> String
              -> IO (Either (ParseError Char Dec) (Double, [Double], [Double]))

parsePaganini spec = parseFromFile (paganiniStmt spec)
