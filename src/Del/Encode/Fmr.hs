module Del.Encode.Fmr where

import Data.Ratio (numerator, denominator)

import Del.Encode.Utils
import Del.Syntax

encodeExp :: Exp -> String
encodeExp (Num x) = case (,) <$> numerator <*> denominator $ toRational x of
                      (n,1) -> show n
                      (n,d) -> show n ++ "/" ++ show d
encodeExp (Sym n _ ds) = n ++ encodeDiff ds
encodeExp (Neg e) = "-" ++ encodeExp e
encodeExp (Mul e1 e2) = encodeExp e1 ++ "*" ++ encodeExp e2
encodeExp (Div e1 e2) = encodeExp e1 ++ "/" ++ encodeExp e2
encodeExp (Add e1 e2) = encodeExp e1 ++ " + " ++ encodeExp e2
encodeExp (Sub e1 e2) = encodeExp e1 ++ " - " ++ encodeExp e2
encodeExp (Pow e1 e2) = "(" ++ encodeExp e1 ++ ")**(" ++ encodeExp e2 ++ ")"
