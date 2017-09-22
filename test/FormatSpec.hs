module FormatSpec where

import Test.Hspec

import Format
import Syntax

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "diffs in one-dimension" $ do
        it "checks dii" $ dii 1 X `shouldBe` "2*(a[i+1] + a[i-1] - 2*a[i] - h*(a_x[i+1] - a_x[i-1])/4)/h/h"
        it "checks diii" $ diii 1 X `shouldBe` "(a_x[i+1] + a_x[i-1] - 2*a_x[i])/h/h"
    describe "diffs in two-dimension" $ do
        it "checks dij" $ dij 2 [X,Y] `shouldBe` "(a[i+1,j+1] + a[i-1,j-1] - a[i+1,j-1] - a[i-1,j+1])/2/h/h - (a_x[i+1,j+1] - a_x[i-1,j-1] - a_x[i+1,j-1] + a_x[i-1,j+1] + a_y[i+1,j+1] - a_y[i-1,j-1] + a_y[i+1,j-1] - a_y[i-1,j+1])/8/h"
        it "checks diij" $ diij 2 [X,Y] `shouldBe` "(a_y[i+1,j] + a_y[i-1,j] - 2*a_y[i,j])/h/h"
