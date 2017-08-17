{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Syntax where

import Data.Hashable (Hashable(..))
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import GHC.Generics (Generic)

type EOM = [Equation]
data Equation = Equation { lhs :: Exp
                         , rhs :: Exp
                         } deriving Show

data Exp = Num Double
         | Sym { name :: String
               , dependOn :: Arg
               , differentiatedBy :: Coords
               }
         | Neg Exp
         | Add Exp Exp
         | Mul Exp Exp
         | Sub Exp Exp
         | Div Exp Exp
         | Pow Exp Exp
         deriving (Show, Eq, Ord)

type Arg = S.Set Coord
type Coords = MS.MultiSet Coord
data Coord = T | X | Y | Z deriving (Eq, Ord, Generic)

instance Show Coord where
    show T = "t"
    show X = "x"
    show Y = "y"
    show Z = "z"

instance Hashable Coord

instance Hashable Arg where
    hashWithSalt i = hashWithSalt i . S.toList

instance Hashable Coords where
    hashWithSalt i = hashWithSalt i . MS.toList
