module Del.Encode.Cpp where

import Del.Encode.Utils
import Del.Syntax

encodeExp :: Exp -> String
encodeExp (Num x) = show x
encodeExp (Sym n _ ds) = n ++ encodeDiff ds
encodeExp (Neg e) = "-" ++ encodeExp e
encodeExp (Mul e1 e2) = encodeExp e1 ++ "*" ++ encodeExp e2
encodeExp (Div e1 e2) = encodeExp e1 ++ "/" ++ encodeExp e2
encodeExp (Add e1 e2) = encodeExp e1 ++ " + " ++ encodeExp e2
encodeExp (Sub e1 e2) = encodeExp e1 ++ " - " ++ encodeExp e2
encodeExp (Pow e1 e2) = "pow(" ++ encodeExp e1 ++ "," ++ encodeExp e2 ++ ")"
