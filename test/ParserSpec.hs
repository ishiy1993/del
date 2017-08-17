module ParserSpec where

import Control.Lens
import Test.Hspec
import Text.Trifecta

import Parser
import Syntax

spec :: Spec
spec = describe "parseExp" $ do
    it "checks a+b" $
        parseString parseExp mempty "a+b" ^?! _Success
            `shouldBe` Add (Sym "a" mempty mempty) (Sym "b" mempty mempty)
    it "checks a-b" $
        parseString parseExp mempty "a-b" ^?! _Success
            `shouldBe` Sub (Sym "a" mempty mempty) (Sym "b" mempty mempty)
    it "checks a*b" $
        parseString parseExp mempty "a*b" ^?! _Success
            `shouldBe` Mul (Sym "a" mempty mempty) (Sym "b" mempty mempty)
    it "checks a/b" $
        parseString parseExp mempty "a/b" ^?! _Success
            `shouldBe` Div (Sym "a" mempty mempty) (Sym "b" mempty mempty)
    it "checks a**b" $
        parseString parseExp mempty "a**b" ^?! _Success
            `shouldBe` Pow (Sym "a" mempty mempty) (Sym "b" mempty mempty)
    it "checks a*b+c" $
        parseString parseExp mempty "a*b+c" ^?! _Success
            `shouldBe` Add (Mul (Sym "a" mempty mempty) (Sym "b" mempty mempty)) (Sym "c" mempty mempty)
    it "checks a/b-c" $
        parseString parseExp mempty "a/b-c" ^?! _Success
            `shouldBe` Sub (Div (Sym "a" mempty mempty) (Sym "b" mempty mempty)) (Sym "c" mempty mempty)
    it "checks a*b**c" $
        parseString parseExp mempty "a*b**c" ^?! _Success
            `shouldBe` Mul (Sym "a" mempty mempty) (Pow (Sym "b" mempty mempty) (Sym "c" mempty mempty))
    it "checks a**b*c" $
        parseString parseExp mempty "a**b*c" ^?! _Success
            `shouldBe` Mul (Pow (Sym "a" mempty mempty) (Sym "b" mempty mempty)) (Sym "c" mempty mempty)
    it "checks -a+b" $
        parseString parseExp mempty "-a+b" ^?! _Success
            `shouldBe` Add (Neg (Sym "a" mempty mempty)) (Sym "b" mempty mempty)
    it "checks -a*b" $
        parseString parseExp mempty "-a*b" ^?! _Success
            `shouldBe` Mul (Neg (Sym "a" mempty mempty)) (Sym "b" mempty mempty)
    it "checks -(a**b)" $
        parseString parseExp mempty "-(a**b)" ^?! _Success
            `shouldBe` Neg (Pow (Sym "a" mempty mempty) (Sym "b" mempty mempty))
