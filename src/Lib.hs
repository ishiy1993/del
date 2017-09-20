{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lib where

import Control.Arrow (first)
import Control.Applicative (liftA2)
import Data.Function (fix)
import Data.List (sortOn, groupBy)
import Data.Monoid (Any(..))
import Data.Profunctor (dimap)
import qualified Data.Set as S
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

simplify :: Exp -> Exp
simplify = buildup . compose . eval . flatten . expand . cleanup

type Term = M.Map Exp Double

travel :: (a -> (Bool, a)) -> a -> a
travel worker = fix (\f e -> let (b,e') = worker e in if b then f e' else e)

-- 無駄な項を消し, 構文要素を減らす
cleanup :: Exp -- [Add, Sub, Mul, Div, Pow, Neg, Sym, Num]
        -> Exp -- [Add, Mul, Pow, Sym, Num]
cleanup = travel worker
  where
    worker2 f = let g = first Any . worker
                 in dimap (\(a,b)->(g a,g b)) (first getAny) (uncurry (liftA2 f))
    m1 = Num (-1)
    worker (Add (Num 0) e) = (True, e)
    worker (Add e (Num 0)) = (True, e)
    worker (Add e1 e2) = worker2 Add (e1,e2)
    worker (Sub e1 e2) = (True, Add e1 (Mul m1 e2))
    worker (Mul (Num 1) e) = (True, e)
    worker (Mul e (Num 1)) = (True, e)
    worker (Mul (Num 0) _) = (True, Num 0)
    worker (Mul _ (Num 0)) = (True, Num 0)
    worker (Mul e1 e2) = worker2 Mul (e1,e2)
    worker (Div e1 e2) = (True, Mul e1 (Pow e2 m1))
    worker (Pow e1 (Num 1)) = (True, e1)
    worker (Pow _ (Num 0)) = (True, Num 1)
    worker (Pow e1 e2) = worker2 Pow (e1,e2)
    worker (Neg e) = (True, Mul m1 e)
    worker e = (False, e)

-- 積の和の形にする
expand :: Exp -> Exp
expand = travel worker
  where
    worker2 f = let g = first Any . worker
                 in dimap (\(a,b)->(g a,g b)) (first getAny) (uncurry (liftA2 f))
    worker (Add e1 e2) = worker2 Add (e1,e2)
    worker (Mul e1 (Add e2 e3)) = (True, Add (Mul e1 e2) (Mul e1 e3))
    worker (Mul (Add e1 e2) e3) = (True, Add (Mul e1 e3) (Mul e2 e3))
    worker (Mul e1 e2) = worker2 Mul (e1,e2)
    worker (Pow (Mul e1 e2) e3) = (True, Mul (Pow e1 e3) (Pow e2 e3))
    worker (Pow e1 e2) = worker2 Pow (e1,e2)
    worker e = (False, e)

flatten :: Exp -> [(Double, Term)]
flatten (Add e1 e2) = flatten e1 ++ flatten e2
flatten (Mul e1 e2) = merge (flatten e1) (flatten e2)
flatten (Pow e1@(Add _ _) (Num n)) = [(1, M.singleton e1 n)]
flatten (Pow (Pow e1 (Num m)) (Num n)) = flatten $ Pow e1 (Num $ n * m)
flatten (Pow e1 (Num n)) = [(1, M.singleton e1 n)]
flatten s@Sym{} = [(1, M.singleton s 1.0)]
flatten (Num x) = [(x, M.empty)]
flatten _ = []

merge :: [(Double, Term)] -> [(Double, Term)] -> [(Double, Term)]
merge e1 e2 = [foldr (\(a,t) (a',t')->(a*a', M.unionWith (+) t t')) (1,M.empty) $ e1++e2]

-- 同じTermをまとめる
eval :: [(Double,Term)] -> [(Double, Term)]
eval = foldr (\xs acc -> eval' xs:acc) [] . groupBy (\x y -> snd x == snd y) . sortOn snd
    where eval' = (,) <$> sum . map fst <*> snd . head

compose :: [(Double,Term)] -> Exp
compose = foldr1 Add . map build
  where
    build (x,t) | x == 0 = Num 0
                | M.null t = Num x
                | x == 1 = buildTerm t
                | otherwise = Mul (Num x) (buildTerm t)
    buildTerm = foldr1 Mul . M.foldrWithKey worker []
      where
        worker e n acc | n == 0 = Num 1:acc
                       | n == 1 = e:acc
                       | otherwise = Pow e (Num n):acc

buildup :: Exp -- [Add, Mul, Pow, Sym, Num]
        -> Exp -- [Add, Sub, Mul, Div, Pow, Neg, Sym, Num]
buildup = travel worker
  where
    worker2 f = let g = first Any . worker
                 in dimap (\(a,b)->(g a,g b)) (first getAny) (uncurry (liftA2 f))
    worker (Add (Num 0) e) = (True, e)
    worker (Add e (Num 0)) = (True, e)
    worker (Add e1 (Neg e2)) = (True, Sub e1 e2)
    worker (Add e1 e2) = worker2 Add (e1,e2)
    worker (Sub e1 (Neg e2)) = (True, Add e1 e2)
    worker (Sub e1 e2) = worker2 Sub (e1,e2)
    worker (Mul (Num 1) e) = (True, e)
    worker (Mul e (Num 1)) = (True, e)
    worker (Mul (Num 0) _) = (True, Num 0)
    worker (Mul _ (Num 0)) = (True, Num 0)
    worker (Mul (Neg e1) e2) = (True, Neg (Mul e1 e2))
    worker (Mul e1 (Neg e2)) = (True, Neg (Mul e1 e2))
    worker (Mul e1 e2) = worker2 Mul (e1,e2)
    worker (Div (Neg e1) e2) = (True, Neg (Div e1 e2))
    worker (Div e1 (Neg e2)) = (True, Neg (Div e1 e2))
    worker (Div e1 e2) = worker2 Div (e1,e2)
    worker (Pow (Num 1) _) = (True, Num 1)
    worker (Pow _ (Num 0)) = (True, Num 1)
    worker (Pow (Num 0) _) = (True, Num 0)
    worker (Pow e1 e2) = worker2 Pow (e1,e2)
    worker (Neg (Neg e)) = (True, e)
    worker (Num n) | n < 0 = (True, Neg (Num $ negate n))
    worker e = (False, e)
