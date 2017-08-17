module Parser where

import Control.Applicative
import Control.Exception
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Data.Typeable
import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Style

import Syntax

parseEOM :: Parser EOM
parseEOM = many parseEquation

parseEquation :: Parser Equation
parseEquation = do
    spaces
    l <- parseSym
    spaces
    char '='
    spaces
    r <- parseExp
    spaces
    return $ Equation l r

parseExp :: Parser Exp
parseExp = expr

-- この実装は正しくない
-- `*`, `/` よりも `**` のほうが結合性が高くなっているが
-- `-c**2` を `-(c**2)` ではなく `(-c)**2` と解釈してしまう。
expr :: Parser Exp
expr = (factor `chainl1` powop) `chainl1` mulop `chainl1` addop
    where addop = infixOp "+" Add <|> infixOp "-" Sub
          mulop = infixOp "*" Mul <|> infixOp "/" Div
          powop = infixOp "**" Pow

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp op f = f <$ symbol op

factor :: Parser Exp
factor = parens expr
     <|> neg
     <|> parseSym
     <|> parseNum

neg :: Parser Exp
neg = symbol "-" >> Neg <$> factor

parseNum :: Parser Exp
parseNum = Num . either fromIntegral id <$> integerOrDouble

parseSym :: Parser Exp
parseSym = do
    n <- some letter
    as <- option S.empty $ brackets args
    ds <- option MS.empty $ MS.fromList <$> (char '_' *> some coord)
    return $ Sym n as ds

args :: Parser Arg
args = S.fromList <$> some (coord <* optional comma)

coord :: Parser Coord
coord = (char 't' *> pure T)
    <|> (char 'x' *> pure X)
    <|> (char 'y' *> pure Y)
    <|> (char 'z' *> pure Z)

getEOMFromFile :: String -> IO EOM
getEOMFromFile fn = do
    str <- readFile fn
    let str' = filter (/=' ') str
    case parseString parseEOM mempty str' of
         Success eom -> return eom
         Failure err -> throwIO $ ParseException err

data ParseException = ParseException ErrInfo
    deriving Typeable

instance Show ParseException where
    show (ParseException e) = show e

instance Exception ParseException
