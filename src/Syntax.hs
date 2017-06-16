module Syntax where

import qualified Data.Set as S
import qualified Data.MultiSet as MS

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
         deriving (Show, Eq)

type Arg = S.Set Coord
type Coords = MS.MultiSet Coord
data Coord = T | X | Y | Z deriving (Eq, Ord)

instance Show Coord where
    show T = "t"
    show X = "x"
    show Y = "y"
    show Z = "z"

