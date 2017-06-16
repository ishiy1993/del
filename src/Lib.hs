module Lib where

import Control.Applicative
import Data.Char (toLower)
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Style

type EOM = [Equation]
data Equation = Equation { lhs :: Exp
                         , rhs :: Exp
                         } deriving Show
data Exp = Num Double
         | Term { name :: String
                , dependOn :: Arg
                , differentiatedBy :: Coords
                }
         | Add Exp Exp
         | Mul Exp Exp
         | Sub Exp Exp
         | Div Exp Exp
         | Pow Exp Exp
         deriving Show

type Arg = S.Set Coord
type Coords = MS.MultiSet Coord
data Coord = T | X | Y | Z deriving (Eq, Ord)

instance Show Coord where
    show T = "t"
    show X = "x"
    show Y = "y"
    show Z = "z"

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
    -- eof <|> (newline *> pure ())
    return $ Equation l r

parseExp :: Parser Exp
parseExp = parens expr
       <|> parseTerm
       <|> parseNum

expr :: Parser Exp
expr = buildExpressionParser table parseExp
    where
        table = [[binary "^" Pow AssocRight]
                ,[binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
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

formatEOM :: EOM -> String
formatEOM = unlines . map formatEquation

formatEquation :: Equation -> String
formatEquation (Equation l r) = unwords [formatExp l, "=", formatExp r]

formatExp :: Exp -> String
formatExp (Num x) = show x
formatExp (Term n a d) = n ++ formatArg a ++ formatDiff d
formatExp (Pow e1 e2) = "(" ++ formatExp e1 ++ " ^ " ++ formatExp e2 ++")"
formatExp (Mul e1 e2) = "(" ++ formatExp e1 ++ " * " ++ formatExp e2 ++")"
formatExp (Div e1 e2) = "(" ++ formatExp e1 ++ " / " ++ formatExp e2 ++")"
formatExp (Add e1 e2) = "(" ++ formatExp e1 ++ " + " ++ formatExp e2 ++")"
formatExp (Sub e1 e2) = "(" ++ formatExp e1 ++ " - " ++ formatExp e2 ++")"

formatArg :: Arg -> String
formatArg as | S.null as = ""
             | otherwise = show $ S.toList as

formatDiff :: Coords -> String
formatDiff cs | MS.null cs = ""
              | otherwise = "_" ++ concatMap show (MS.toAscList cs)

