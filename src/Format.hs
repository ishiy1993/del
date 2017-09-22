{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Format where

import Data.Ord (comparing)
import Data.List (intercalate, maximumBy, sortBy, unfoldr)
import Data.Ratio (numerator, denominator)
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Text.Printf

import Lib
import Syntax

toCode :: EOM -> String
toCode eom' = unlines
  [ "dimension :: " ++ show dim
  , "axes :: " ++ intercalate "," (map show axes)
  , ""
  , "double :: cfl"
  , "double :: s"
  , "double :: h"
  , "double :: dt = cfl*h"
  , ""
  , defDiffs axes
  , ""
  , defSmoo dim
  , ""
  , defFuns eom axes
  , defInit eom axes
  , defStep eom axes dim
  ]
  where
    dim = length axes
    eom = simplify eom'
    axes = S.toList $ S.delete T $ maximumBy (comparing S.size) $ map (dependOn . lhs) eom

encodeDiff :: Coords -> String
encodeDiff cs | MS.null cs = ""
              | otherwise = "_" ++ concatMap show (MS.toAscList cs)

defDiffs :: [Coord] -> String
defDiffs axes = joinLines $ defDiff2 axes ++ defDiff3 axes

defDiff :: [Coord] -> Coords -> String
defDiff as ds = unwords [l, "=", r]
    where l = "d" ++ encodeDiff ds
          r = "fun" ++ paren ("a" : map (\i -> "a_" ++ show i) as) ++ " " ++ defDiffBody (length as) ds

defDiffBody :: Int -> Coords -> String
defDiffBody dim ds
    | typeOf ds == (2,1) = dii dim $ head (MS.distinctElems ds)
    | typeOf ds == (2,2) = dij dim $ MS.distinctElems ds
    | typeOf ds == (3,1) = diii dim $ head (MS.distinctElems ds)
    | typeOf ds == (3,2) = diij dim $ fromMS ds
    | otherwise = dijk
    where
        typeOf ds = (MS.size ds, MS.distinctSize ds)
        fromMS = map fst . sortBy (comparing (negate.snd)) . MS.toOccurList

data Index = Pre | Zero | Succ

instance Show Index where
    show Pre = "-1"
    show Zero = ""
    show Succ = "+1"

class ShowIndex a where
    type family Form a :: *
    showIndex :: Int -> Form a -> a -> String

instance ShowIndex Index where
    type Form Index = Coord
    showIndex dim i di | dim == 1 = bracket ["i" ++ show di]
                       | dim == 2 && i == X = bracket ["i" ++ show di, "j"]
                       | dim == 2 && i == Y = bracket ["i", "j" ++ show di]
                       | dim == 3 && i == X = bracket ["i" ++ show di, "j", "k"]
                       | dim == 3 && i == Y = bracket ["i", "j" ++ show di, "k"]
                       | dim == 3 && i == Z = bracket ["i", "j", "k" ++ show di]

instance ShowIndex (Index,Index) where
    type Form (Index,Index) = (Coord, Coord)
    showIndex dim (i,j) (di,dj) | dim == 2 = bracket ["i" ++ show di, "j" ++ show dj]
                                | dim == 3 && (i,j) == (X,Y) = bracket ["i" ++ show di, "j" ++ show dj, "k"]
                                | dim == 3 && (i,j) == (X,Z) = bracket ["i" ++ show di, "j", "k" ++ show dj]
                                | dim == 3 && (i,j) == (Y,Z) = bracket ["i", "j" ++ show di, "k" ++ show dj]

dii :: Int -> Coord -> String
dii dim i = printf "2*(%s + %s - 2*%s - h*(%s - %s)/4)/h/h"
                   (a Succ) (a Pre) (a Zero) (ai Succ) (ai Pre)
    where
        a ix = "a" ++ showIndex dim i ix
        ai ix = "a_" ++ show i ++ showIndex dim i ix

dij :: Int -> [Coord] -> String
dij dim [i,j] = printf "(%s + %s - %s - %s)/2/h/h - (%s - %s - %s + %s + %s - %s + %s - %s)/8/h"
                       (a Succ Succ) (a Pre Pre) (a Succ Pre) (a Pre Succ)
                       (ai Succ Succ) (ai Pre Pre) (ai Succ Pre) (ai Pre Succ)
                       (aj Succ Succ) (aj Pre Pre) (aj Succ Pre) (aj Pre Succ)
    where
        a di dj = "a" ++ showIndex dim (i,j) (di,dj)
        ai di dj = "a_" ++ show i ++ showIndex dim (i,j) (di,dj)
        aj di dj = "a_" ++ show j ++ showIndex dim (i,j) (di,dj)

diii :: Int -> Coord -> String
diii dim i = printf "(%s + %s - 2*%s)/h/h" (ai Succ) (ai Pre) (ai Zero)
    where
        ai ix = "a_" ++ show i ++ showIndex dim i ix

diij :: Int -> [Coord] -> String
diij dim [i,j] = printf "(%s + %s - 2*%s)/h/h" (aj Succ) (aj Pre) (aj Zero)
    where
        aj ix = "a_" ++ show j ++ showIndex dim i ix

dijk :: String
dijk = "(a_z[i+1,j+1,k] + a_z[i-1,j-1,k] - a_z[i+1,j-1,k] - a_z[i-1,j+1,k])/4/h/h"

defDiff2 :: [Coord] -> [String]
defDiff2 axes = map (defDiff axes) [MS.fromList [i,j] | i <- axes, j <- filter (>=i) axes]

defDiff3 :: [Coord] -> [String]
defDiff3 axes = map (defDiff axes) [MS.fromList [i,j,k] | i <- axes, j <- filter (>=i) axes, k <- filter (>=j) axes]

defSmoo :: Int -> String
defSmoo dim = "smoo = fun(a) " ++ body dim
    where
        body 1 = "-s*a[i] + s*(a[i+1] + a[i-1])/2"
        body 2 = "-s*a[i,j] + s*(a[i+1,j] + a[i-1,j] + a[i,j+1] + a[i,j-1])/4"
        body 3 = "-s*a[i,j,k] + s*(a[i+1,j,k] + a[i-1,j,k] + a[i,j+1,k] + a[i,j-1,k] + a[i,j,k+1] + a[i,j,k-1])/6"

defFuns :: EOM -> [Coord] -> String
defFuns eom axes = joinLines $ map encode es ++ concatMap (\i -> map (encode . diffBy i) es) axes
    where
        eomT = diffByT eom
        es = [eom, eomT]

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
leftHandSide = paren . map (\(Sym n _ ds) -> n ++ encodeDiff ds)

rightHandSide :: [Exp] -> String
rightHandSide vs = funName ++ paren args
    where
        funName = "d" ++ encodeDiff (differentiatedBy $ head vs)
        ns = map name vs
        as = S.toList $ S.delete T $ dependOn $ head vs
        args = paren ns : [paren (map (++"_"++show a) ns) | a <- as]

encodeEquation :: Equation -> String
encodeEquation (Equation l r) = unwords [encodeExp l, "=", encodeExp r]

encodeExp :: Exp -> String
encodeExp (Num x) = i
    where i = case (,) <$> numerator <*> denominator $ toRational x of
                (n,1) -> show n
                (n,d) -> show n ++ "/" ++ show d
encodeExp (Sym n _ ds) = n ++ encodeDiff ds
encodeExp (Neg e) = "-" ++ encodeExp e
encodeExp (Mul e1 e2) = encodeExp e1 ++ "*" ++ encodeExp e2
encodeExp (Div e1 e2) = encodeExp e1 ++ "/" ++ encodeExp e2
encodeExp (Add e1 e2) = encodeExp e1 ++ " + " ++ encodeExp e2
encodeExp (Sub e1 e2) = encodeExp e1 ++ " - " ++ encodeExp e2
encodeExp (Pow e1 e2) = "(" ++ encodeExp e1 ++ ")**(" ++ encodeExp e2 ++ ")"

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
        l = n ++ encodeDiff ds
        r = "d" ++ encodeDiff ds ++ paren (n : map (\i->n++"_"++show i) as)

defInit :: EOM -> [Coord] -> String
defInit = defMainFun (\vs -> unwords [paren vs, "=", "init()"])
                     (\vs -> "double [] :: " ++ intercalate "," (map (++" = 0") vs))

defStep :: EOM -> [Coord] -> Int -> String
defStep eom axes dim = defMainFun header body eom axes
    where
        header vs = unwords [paren $ map (++"'") vs, "=", "step" ++ paren vs]
        body vs = joinLines [ toQ
                            , diffs
                            , updateQs
                            , updateQps
                            , updateQhs
                            , fromQ
                            ]
            where
                addDiffs = id:map (\i q -> q ++ "_" ++ show i) axes
                qs = addDiffs <*> ["q","qp","qh"]
                vss = unfoldr (\xs -> if null xs then Nothing else Just (splitAt (dim+2) xs)) vs
                toQ = unlines $ zipWith (\q v->unwords [q,"=",paren v]) qs vss
                diffs = let as = paren $ addDiffs <*> ["qp"]
                            is = id:[(++show i) | i <- axes] <*> map ("_"++) ["t","tt"]
                         in unlines $ map (\i -> unwords ["q"++i,"=","d"++i++as]) is
                updateQs = unlines $ map updateQ (Nothing:map Just axes)
                updateQ Nothing = "q' = qh + dt*q_t/2 - dt*dt*q_tt/12"
                updateQ (Just i) = let i' = show i in "q_"++i'++"' = qh_"++i'++" + dt*q_t"++i'++"/2 - dt*dt*q_tt"++i'++"/12"
                updateQps = unlines $ map updateQp (Nothing:map Just axes)
                updateQp Nothing = "qp' = q' + dt*q_t + dt*dt*q_tt/2 + smoo(qp)"
                updateQp (Just i) = let i' = show i in "qp_"++i'++"' = q_"++i'++"' + dt*q_t"++i'++" + dt*dt*q_tt"++i'++"/2 + smoo(qp_"++i'++")"
                updateQhs = unlines $ map updateQh (Nothing:map Just axes)
                updateQh Nothing = "qh' = q' + dt*q_t/2 + dt*dt*q_tt/12 + smoo(qh)"
                updateQh (Just i) = let i' = show i in "qh_"++i'++"' = q_"++i'++"' + dt*q_t"++i'++"/2 + dt*dt*q_tt"++i'++"/12 + smoo(qh_"++i'++")"
                fromQ = joinLines $ zipWith (\q v->unwords [paren $ map (++"'") v, "=", q++"'"]) qs vss

defFun :: String -> String -> String
defFun h b = unlines ["begin function " ++ h, b, "end function"]

defMainFun :: ([String] -> String) -> ([String] -> String) -> EOM -> [Coord] -> String
defMainFun h b eom axes = defFun (h vs'') (b vs'')
    where addPostfix p (Sym n as ds) = Sym (n++p) as ds
          vs = map ((\e->e{differentiatedBy = MS.empty}) . lhs) eom
          vs' = id:map d axes <*> ([id, addPostfix "p", addPostfix "h"] <*> vs)
          vs'' = map encodeExp vs'

paren :: [String] -> String
paren ss = "(" ++ intercalate "," ss ++ ")"

bracket :: [String] -> String
bracket ss = "[" ++ intercalate "," ss ++ "]"

joinLines :: [String] -> String
joinLines = intercalate "\n"
