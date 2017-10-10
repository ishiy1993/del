module ParserSpec where

import Control.Lens
import Test.Hspec
import Text.Trifecta

import Parser
import Syntax

check :: Parser a -> String -> a
check parser input = parseString parser mempty input ^?! _Success

spec :: Spec
spec = do
  describe "symParser" $ do
    it "check a" $
      check symParser "a" `shouldBe` (Sym "a" mempty mempty)
    it "check a1" $
      check symParser "a1" `shouldBe` (Sym "a1" mempty mempty)
  describe "expParser" $ do
    let a = Sym "a" mempty mempty
        b = Sym "b" mempty mempty
        c = Sym "c" mempty mempty
    it "checks a+b" $
      check expParser "a+b" `shouldBe` Add a b
    it "checks a-b" $
      check expParser "a-b" `shouldBe` Sub a b
    it "checks a*b" $
      check expParser "a*b" `shouldBe` Mul a b
    it "checks a/b" $
      check expParser "a/b" `shouldBe` Div a b
    it "checks a**b" $
      check expParser "a**b" `shouldBe` Pow a b
    it "checks a*b+c" $
      check expParser "a*b+c" `shouldBe` Add (Mul a b) c
    it "checks a/b-c" $
      check expParser "a/b-c" `shouldBe` Sub (Div a b) c
    it "checks a*b**c" $
      check expParser "a*b**c" `shouldBe` Mul a (Pow b c)
    it "checks a**b*c" $
      check expParser "a**b*c" `shouldBe` Mul (Pow a b) c
    it "checks -a+b" $
      check expParser "-a+b" `shouldBe` Add (Neg a) b
    it "checks -a*b" $
      check expParser "-a*b" `shouldBe` Mul (Neg a) b
    it "checks -(a**b)" $
      check expParser "-(a**b)" `shouldBe` Neg (Pow a b)
