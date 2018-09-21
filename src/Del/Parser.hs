module Del.Parser where

import Control.Applicative
import Control.Exception
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Data.Typeable
import Text.Trifecta

import Del.Syntax

eomParser :: Parser EOM
eomParser = many equationParser

equationParser :: Parser Equation
equationParser = do
    spaces
    l <- symParser
    spaces
    char '='
    spaces
    r <- expParser
    spaces
    return $ Equation l r

expParser :: Parser Exp
expParser = expr

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
     <|> symParser
     <|> numParser

neg :: Parser Exp
neg = symbol "-" >> Neg <$> factor

numParser :: Parser Exp
numParser = Num . either fromIntegral id <$> integerOrDouble

symParser :: Parser Exp
symParser = do
    n <- (:) <$> letter <*> many alphaNum
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

parseEOM :: String -> Either ErrInfo EOM
parseEOM str = case parseString eomParser mempty $ filter (/=' ') str of
                 Success eom -> Right eom
                 Failure err -> Left err

getEOMFromFile :: String -> IO EOM
getEOMFromFile fn = do
  str <- readFile fn
  case parseEOM str of
    Right eom -> return eom
    Left err -> throwIO $ ParseException err

data ParseException = ParseException ErrInfo
    deriving Typeable

instance Show ParseException where
    show (ParseException e) = show e

instance Exception ParseException
