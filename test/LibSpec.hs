module LibSpec where

import Test.Hspec

import Del.Lib
import Del.Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "simplifyExp" $ do
    let a = Sym "a" mempty mempty
    it "checks a+a=2a" $
      simplifyExp (Add a a) `shouldBe` (Mul (Num 2.0) a)
    it "checks a-a=0" $
      simplifyExp (Sub a a) `shouldBe` Num 0
    it "checks a*a=a**2" $
      simplifyExp (Mul a a) `shouldBe` (Pow a (Num 2.0))
    it "checks a/a=1" $
      simplifyExp (Div a a) `shouldBe` Num 1
    it "checks a/(a*a)=a**(-1)" $
      simplifyExp (Div a (Mul a a)) `shouldBe` (Pow a (Neg (Num 1.0)))
    it "checks a/(a/a)=a" $
      simplifyExp (Div a (Div a a)) `shouldBe` a
