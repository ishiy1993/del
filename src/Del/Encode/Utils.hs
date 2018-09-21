module Del.Encode.Utils where

import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.MultiSet as MS

import Del.Syntax

encodeDiff :: Coords -> String
encodeDiff cs | MS.null cs = ""
              | otherwise = "_" ++ concatMap show (MS.toAscList cs)

encodeDiff' :: Coords -> String
encodeDiff' cs | MS.null cs = ""
               | otherwise = "_{" ++ concatMap show (MS.toAscList cs) ++ "}"

encodeArgs :: Arg -> String
encodeArgs as | S.null as = ""
              | otherwise = "[" ++ intercalate "," (map show $ S.toList as) ++ "]"
