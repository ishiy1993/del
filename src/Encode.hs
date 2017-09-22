module Encode where

import qualified Encode.Cpp as Cpp
import qualified Encode.Fmr as Fmr
import qualified Encode.Tex as Tex
import qualified Encode.Txt as Txt
import Syntax

data Lang = Txt
          | Fmr
          | Cpp
          | Tex

encode :: Lang -> EOM -> String
encode lang = unlines . map (encodeEquation lang)

encodeEquation :: Lang -> Equation -> String
encodeEquation lang (Equation l r) = unwords [encodeExp l, "=", encodeExp r]
  where
    encodeExp = case lang of
                  Txt -> Txt.encodeExp
                  Fmr -> Fmr.encodeExp
                  Cpp -> Cpp.encodeExp
                  Tex -> Tex.encodeExp
