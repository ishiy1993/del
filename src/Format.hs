module Format where

import Data.Char (toLower)
import qualified Data.Set as S
import qualified Data.MultiSet as MS

import Syntax

formatEOM :: EOM -> String
formatEOM = unlines . map formatEquation

formatEquation :: Equation -> String
formatEquation (Equation l r) = unwords [formatExp l, "=", formatExp r]

formatExp :: Exp -> String
formatExp (Num x) = show x
formatExp (Term n a ds) = n ++ formatArg a ++ formatDiff ds
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
