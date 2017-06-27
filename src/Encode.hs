module Encode where

import Data.Ord (comparing)
import Data.List (intercalate, maximumBy)
import qualified Data.Set as S
import qualified Data.MultiSet as MS

import Format
import Lib
import Syntax

toCode :: EOM -> String
toCode eom = unlines $
    [ "dimension :: " ++ show dim
    , "axes :: " ++ intercalate "," (map show axes)
    , ""
    , encode eom
    , encode eomT
    ] ++ concatMap (\i -> map (encode . diffBy i) [eom, eomT]) axes
    where
        dim = length axes
        axes = S.toList $ S.delete T $ maximumBy (comparing S.size) $ map (dependOn . lhs) eom
        eomT = diffByT eom

encode :: EOM -> String
encode eom = unlines $ [header]
                     ++ diffs
                     ++ map encodeEquation eom
                     ++  [footer]
    where
        vs = map lhs eom
        diffs = concatMap mkDiff vs
        header = unwords ["begin","function"
                         , leftHandSide vs, "=", rightHandSide vs]
        footer = "end function"

leftHandSide :: [Exp] -> String
leftHandSide = paren . map (\(Sym n _ ds) -> n ++ formatDiff ds)

rightHandSide :: [Exp] -> String
rightHandSide vs = funName ++ paren args
    where
        funName = "d" ++ formatDiff (differentiatedBy $ head vs)
        ns = map name vs
        as = S.toList $ S.delete T $ dependOn $ head vs
        args = paren ns : [paren (map (++"_"++show a) ns) | a <- as]

encodeEquation :: Equation -> String
encodeEquation (Equation l r) = unwords [encodeExp l, "=", encodeExp r]

encodeExp :: Exp -> String
encodeExp (Num x) = show x
encodeExp (Sym n _ ds) = n ++ formatDiff ds
encodeExp (Neg e) = "-" ++ encodeExp e
encodeExp (Mul e1 e2) = "(" ++ encodeExp e1 ++ " * " ++ encodeExp e2 ++")"
encodeExp (Div e1 e2) = "(" ++ encodeExp e1 ++ " / " ++ encodeExp e2 ++")"
encodeExp (Add e1 e2) = "(" ++ encodeExp e1 ++ " + " ++ encodeExp e2 ++")"
encodeExp (Sub e1 e2) = "(" ++ encodeExp e1 ++ " - " ++ encodeExp e2 ++")"

mkDiff :: Exp -> [String]
mkDiff (Sym n as ds)
    | ord == 2 = d2
    | ord == 3 = d2 ++ d3
    | otherwise = []
    where
        ord = MS.size ds
        as' = S.toList $ S.delete T as
        d2 = map (mkEq n as') [MS.fromList [i,j] | i <- as', j <- filter (>=i) as']
        d3 = map (mkEq n as') [MS.fromList [i,j,k] | i <- as', j <- filter (>=i) as', k <- filter (>=j) as']

mkEq :: String -> [Coord] -> Coords -> String
mkEq n as ds = unwords [l, "=", r]
    where
        l = n ++ formatDiff ds
        r = "d" ++ formatDiff ds ++ paren (n : map (\i->n++"_"++show i) as)

paren :: [String] -> String
paren ss = "(" ++ intercalate "," ss ++ ")"
