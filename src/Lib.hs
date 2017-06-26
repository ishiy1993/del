module Lib (diffByT, diffBy) where

import qualified Data.Set as S
import qualified Data.MultiSet as MS

import Syntax

diffByT :: EOM -> EOM
diffByT eom = map (\(Equation l r) -> Equation (d T l) (simplify $ replace $ d T r)) eom
    where
        replace (Add e1 e2) = Add (replace e1) (replace e2)
        replace (Sub e1 e2) = Sub (replace e1) (replace e2)
        replace (Mul e1 e2) = Mul (replace e1) (replace e2)
        replace (Div e1 e2) = Add (replace e1) (replace e2)
        -- Assume the size of ds is 2 at most
        replace e@(Term n a ds)
            | d0 == ds = find e eom
            | T `MS.member` ds = let ds' = head $ MS.toList (ds MS.\\ d0)
                                 in  d ds' $ find (Term n a d0) eom
            | otherwise = e
            where d0 = MS.singleton T
        replace e = e

find :: Exp -> EOM -> Exp
find e = rhs . head . filter (\eq -> lhs eq == e)

diffBy :: Coord -> EOM -> EOM
diffBy i = map (\(Equation l r) -> Equation (d i l) (simplify $ d i r))

d :: Coord -> Exp -> Exp
d i (Num _) = Num 0.0
d i (Term n a d) | i `S.member` a = Term n a (MS.insert i d)
                 | otherwise = Num 0.0
d i (Mul e1 e2) = Add (Mul (d i e1) e2) (Mul e1 (d i e2))
d i (Div e1 e2) = Sub (Div (d i e1) e2) (Mul (Div e1 (Mul e2 e2)) (d i e2))
d i (Add e1 e2) = Add (d i e1) (d i e2)
d i (Sub e1 e2) = Sub (d i e1) (d i e2)

simplify :: Exp -> Exp
simplify e | simplifible e = simplify $ simplify' e
           | otherwise = e

simplifible :: Exp -> Bool
simplifible (Mul (Num 0) _) = True
simplifible (Mul _ (Num 0)) = True
simplifible (Mul (Num 1) e) = True
simplifible (Mul e (Num 1)) = True
simplifible (Mul e1 e2) = simplifible e1 || simplifible e2
simplifible (Div (Num 0) _) = True
simplifible (Div e (Num 1)) = True
simplifible (Div e1 e2) = simplifible e1 || simplifible e2
simplifible (Add (Num 0) e) = True
simplifible (Add e (Num 0)) = True
simplifible (Add e1 e2) = simplifible e1 || simplifible e2
simplifible (Sub (Num 0) e) = True
simplifible (Sub e (Num 0)) = True
simplifible (Sub e1 e2) = simplifible e1 || simplifible e2
simplifible e = False

-- This is not enough
simplify' :: Exp -> Exp
simplify' (Mul (Num 0) _) = Num 0
simplify' (Mul _ (Num 0)) = Num 0
simplify' (Mul (Num 1) e) = simplify' e
simplify' (Mul e (Num 1)) = simplify' e
simplify' (Mul e1 e2) = Mul (simplify' e1) (simplify' e2)
simplify' (Div (Num 0) _) = Num 0
simplify' (Div e (Num 1)) = simplify' e
simplify' (Div e1 e2) = Div (simplify' e1) (simplify' e2)
simplify' (Add (Num 0) e) = simplify' e
simplify' (Add e (Num 0)) = simplify' e
simplify' (Add e1 e2) = Add (simplify' e1) (simplify' e2)
simplify' (Sub (Num 0) e) = Mul (Num (-1)) (simplify' e)
simplify' (Sub e (Num 0)) = simplify' e
simplify' (Sub e1 e2) = Sub (simplify' e1) (simplify' e2)
simplify' e = e
