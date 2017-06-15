module Lib where

import qualified Data.MultiSet as MS

type EOM = [Equation]
data Equation = Equation { lhs :: Exp
                         , rhs :: Exp
                         }
data Exp = Num Double
         | Term { name :: String
                , dependOn :: Coords
                , derivedBy :: Coords
                }
         | Add Exp Exp
         | Mul Exp Exp
         | Sub Exp Exp
         | Div Exp Exp
         | Pow Exp Exp

type Coords = MS.MultiSet Coord
data Coord = T | X | Y | Z
