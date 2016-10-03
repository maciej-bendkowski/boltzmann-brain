-- | Author: Maciej Bendkowski <maciej.bendkowski@tcs.uj.edu.pl>
module Parser
    ( parseSystem
    , printError
    ) where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map.Strict as M

import qualified System as S

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
    where lineCmnt  = L.skipLineComment "//"
          blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

identifier :: Parser String
identifier = lexeme $ (:) <$> upperChar <*> many (alphaNumChar <|> char '_')

systemStmt :: Parser (S.System Integer)
systemStmt = sc *> systemStmt' <* eof
    where systemStmt' = do
            ds <- some defsStmt
            return S.System { S.defs = M.fromList ds } 

defsStmt :: Parser (String, [S.Cons Integer])
defsStmt = do
    t <- identifier
    void (symbol "=")
    exprs <- exprListStmt
    return (t, exprs)

exprListStmt :: Parser [S.Cons Integer]
exprListStmt = do
    stms <- exprStmt `sepBy` symbol "|"
    void (symbol ".")
    return stms

exprStmt :: Parser (S.Cons Integer)
exprStmt = do
    f <- identifier
    as <- many argStmt
    w <- option 1 (parens integer)
    return S.Cons { S.func = f
                  , S.args = as
                  , S.weight = w
                  } 

argStmt :: Parser S.Arg
argStmt = do
    t <- identifier
    return $ S.Type t

parseFromFile :: Parsec e String a 
              -> String 
              -> IO (Either (ParseError Char e) a)

parseFromFile p file = runParser p file <$> readFile file

parseSystem :: String 
            -> IO (Either (ParseError Char Dec) (S.System Integer))

parseSystem = parseFromFile systemStmt

printError :: ParseError Char Dec -> IO ()
printError err = putStr $ parseErrorPretty err
