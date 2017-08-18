{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lib where

import Data.List (sortOn, groupBy, foldl1')
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)
import qualified Data.MultiSet as MS
import qualified Data.Map as M

import Syntax

diffByT :: EOM -> EOM
diffByT eom = map (\(Equation l r) -> Equation (d T l) (simplify $ replace $ d T r)) eom
    where
        replace (Add e1 e2) = Add (replace e1) (replace e2)
        replace (Sub e1 e2) = Sub (replace e1) (replace e2)
        replace (Mul e1 e2) = Mul (replace e1) (replace e2)
        replace (Div e1 e2) = Div (replace e1) (replace e2)
        replace (Pow e1 e2) = Pow (replace e1) (replace e2)
        replace (Neg e) = Neg (replace e)
        replace e@(Sym n a ds)
            | d0 == ds = find e eom
            | T `MS.member` ds = let ds' = MS.toList (ds MS.\\ d0)
                                 in  foldr d (find (Sym n a d0) eom) ds'
            | otherwise = e
            where d0 = MS.singleton T
        replace e = e

find :: Exp -> EOM -> Exp
find e = rhs . head . filter (\eq -> lhs eq == e)

diffBy :: Coord -> EOM -> EOM
diffBy i = map (\(Equation l r) -> Equation (d i l) (simplify $ d i r))

d :: Coord -> Exp -> Exp
d i (Num _) = Num 0.0
d i (Sym n a d) | i `S.member` a = Sym n a (MS.insert i d)
                | otherwise = Num 0.0
d i (Neg e) = Neg (d i e)
d i (Mul e1 e2) = Add (Mul (d i e1) e2) (Mul e1 (d i e2))
d i (Div e1 e2) = Sub (Div (d i e1) e2) (Mul (Div e1 (Mul e2 e2)) (d i e2))
d i (Add e1 e2) = Add (d i e1) (d i e2)
d i (Sub e1 e2) = Sub (d i e1) (d i e2)
d i (Pow e1 (Num n)) | n /= 0 = Mul (Mul (Num n) (Pow e1 (Num $ n -1))) (d i e1)
                     | otherwise = Num 0.0

-- This is unable to deal with Div
simplify :: Exp -> Exp
simplify = rebuild . eval . flatten . expand . cleanup

type Term = M.Map Exp Double

cleanup :: Exp -> Exp
cleanup e | cleanupable e = cleanup $ cleanup' e
          | otherwise = e
    where
        cleanupable (Add (Num 0) e) = True
        cleanupable (Add e (Num 0)) = True
        cleanupable (Add e1 e2) = cleanupable e1 || cleanupable e2
        cleanupable (Sub (Num 0) e) = True
        cleanupable (Sub e (Num 0)) = True
        cleanupable (Sub e1 e2) = cleanupable e1 || cleanupable e2
        cleanupable (Mul (Num 1) e) = True
        cleanupable (Mul e (Num 1)) = True
        cleanupable (Mul (Num 0) e) = True
        cleanupable (Mul e (Num 0)) = True
        cleanupable (Mul e1 e2) = cleanupable e1 || cleanupable e2
        cleanupable (Div e (Num 1)) = True
        cleanupable (Div (Num 0) e) = True
        cleanupable (Div e1 e2) = cleanupable e1 || cleanupable e2
        cleanupable (Pow e1 (Num 0)) = True
        cleanupable (Pow e1 (Num 1)) = True
        cleanupable (Pow e1 (Neg (Num n))) = True
        cleanupable (Neg e) = True
        cleanupable e = False

        cleanup' (Add (Num 0) e) = cleanup' e
        cleanup' (Add e (Num 0)) = cleanup' e
        cleanup' (Add e1 e2) = Add (cleanup' e1) (cleanup' e2)
        cleanup' (Sub (Num 0) e) = Neg (cleanup' e)
        cleanup' (Sub e (Num 0)) = cleanup' e
        cleanup' (Sub e1 e2) = Sub (cleanup' e1) (cleanup' e2)
        cleanup' (Mul (Num 1) e) = cleanup' e
        cleanup' (Mul e (Num 1)) = cleanup' e
        cleanup' (Mul (Num 0) e) = Num 0
        cleanup' (Mul e (Num 0)) = Num 0
        cleanup' (Mul e1 e2) = Mul (cleanup' e1) (cleanup' e2)
        cleanup' (Div e (Num 1)) = cleanup' e
        cleanup' (Div (Num 0) e) = Num 0
        cleanup' (Div e1 e2) = Mul (cleanup' e1) (Pow (cleanup' e2) (Num $ -1))
        cleanup' (Pow e1 (Num 0)) = Num 1.0
        cleanup' (Pow e1 (Num 1)) = cleanup' e1
        cleanup' (Pow e1 (Neg (Num n))) = Pow (cleanup' e1) (Num $ -n)
        cleanup' (Neg e) = Mul (Num (-1)) (cleanup' e)
        cleanup' e = e

expand :: Exp -> Exp
expand e | expandable e = expand $ expand' e
         | otherwise = e
    where
        expandable (Add e1 e2) = expandable e1 || expandable e2
        expandable (Sub e1 e2) = expandable e1 || expandable e2
        expandable (Mul (Add e1 e2) e) = True
        expandable (Mul e (Add e1 e2)) = True
        expandable (Mul (Sub e1 e2) e) = True
        expandable (Mul e (Sub e1 e2)) = True
        expandable (Mul e1 e2) = expandable e1 || expandable e2
        expandable (Div (Add e1 e2) e) = True
        expandable (Div (Sub e1 e2) e) = True
        expandable (Div e1 e2) = expandable e1 || expandable e2
        expandable e = False

        expand' (Add e1 e2) = Add (expand' e1) (expand' e2)
        expand' (Sub e1 e2) = Sub (expand' e1) (expand' e2)
        expand' (Mul (Add e1 e2) e) = Add (expand' $ Mul e1 e) (expand' $ Mul e2 e)
        expand' (Mul e (Add e1 e2)) = Add (expand' $ Mul e e1) (expand' $ Mul e e2)
        expand' (Mul (Sub e1 e2) e) = Sub (expand' $ Mul e1 e) (expand' $ Mul e2 e)
        expand' (Mul e (Sub e1 e2)) = Sub (expand' $ Mul e e1) (expand' $ Mul e e2)
        expand' (Mul e1 e2) = Mul (expand' e1) (expand' e2)
        expand' (Div (Add e1 e2) e) = Add (expand' $ Div e1 e) (expand' $ Div e2 e)
        expand' (Div (Sub e1 e2) e) = Sub (expand' $ Div e1 e) (expand' $ Div e2 e)
        expand' (Div e1 e2) = Div (expand' e1) (expand' e2)
        expand' e = e

flatten :: Exp -> [(Double, Term)]
flatten (Add e1 e2) = flatten e1 ++ flatten e2
flatten (Sub e1 e2) = flatten e1 ++ flatten (expand $ Mul (Num (-1)) e2)
flatten (Mul e1 e2) = merge (flatten e1) (flatten e2)
flatten (Pow e1 (Num n)) = [(1, M.singleton e1 n)]
flatten s@Sym{} = [(1, M.singleton s 1.0)]
flatten (Num x) = [(x, M.empty)]
flatten _ = []

merge :: [(Double, Term)] -> [(Double, Term)] -> [(Double, Term)]
merge e1 e2 = [foldr (\(a,t) (a',t') -> (a*a', M.unionWith (+) t t')) (1,M.empty) $ e1 ++ e2]

eval :: [(Double,Term)] -> [(Double, Term)]
eval = foldr (\xs acc -> eval' xs:acc) [] . groupBy (\x y -> snd x == snd y) . sortOn snd
    where eval' = (,) <$> sum . map fst <*> snd . head

rebuild :: [(Double, Term)] -> Exp
rebuild = foldl1' build . map fromJust . filter isJust . map toExp
    where
        build e1 (Neg e2) = Sub e1 e2
        build e1 e2 = Add e1 e2
        toExp (x,t) | x == 0 = Nothing
                    | x == 1 = Just $ toExp' t
                    | x == -1 = Just $ Neg $ toExp' t
                    | x < -1 = Just $ Neg $ Mul (Num $ negate x) (toExp' t)
                    | otherwise = Just $ Mul (Num x) (toExp' t)
        toExp' = foldl1' Mul . foldr build' [] . M.toList
        build' (e,i) acc | i == 1 = e:acc
                         | i == 0 = acc
                         | otherwise = Pow e (Num i):acc
