module LibSpec where

import Test.Hspec

import Lib
import Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "simplify" $ do
    let a = Sym "a" mempty mempty
    it "checks a+a=2a" $
      simplify (Add a a) `shouldBe` (Mul (Num 2.0) a)
    it "checks a-a=0" $
      simplify (Sub a a) `shouldBe` Num 0
    it "checks a*a=a**2" $
      simplify (Mul a a) `shouldBe` (Pow a (Num 2.0))
    it "checks a/a=1" $
      simplify (Div a a) `shouldBe` Num 1
    it "checks a/(a*a)=a**(-1)" $
      simplify (Div a (Mul a a)) `shouldBe` (Pow a (Neg (Num 1.0)))
    it "checks a/(a/a)=a" $
      simplify (Div a (Div a a)) `shouldBe` a
