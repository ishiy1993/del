module Parser where

import Control.Applicative
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Style

import Syntax

parseEOM :: Parser EOM
parseEOM = many parseEquation

parseEquation :: Parser Equation
parseEquation = do
    spaces
    l <- parseExp
    spaces
    char '='
    spaces
    r <- parseExp
    spaces
    return $ Equation l r

parseExp :: Parser Exp
parseExp = parens expr
       <|> parseTerm
       <|> parseNum

expr :: Parser Exp
expr = buildExpressionParser table parseExp
    where
        table = [[binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
                ,[binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
                ]
        binary op f = Infix (f <$ reserve emptyOps op)

parseNum :: Parser Exp
parseNum = Num . either fromIntegral id <$> integerOrDouble

parseTerm :: Parser Exp
parseTerm = do
    n <- some letter
    as <- option S.empty $ brackets args
    ds <- option MS.empty $ MS.fromList <$> (char '_' *> some coord)
    return $ Term n as ds

args :: Parser Arg
args = S.fromList <$> some (coord <* optional comma)

coord :: Parser Coord
coord = (char 't' *> pure T)
    <|> (char 'x' *> pure X)
    <|> (char 'y' *> pure Y)
    <|> (char 'z' *> pure Z)

parseEOMFromFile :: String -> IO (Maybe EOM)
parseEOMFromFile = parseFromFile parseEOM
