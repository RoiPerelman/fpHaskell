module TypeClassSpec where

import Test.Hspec
import Test.QuickCheck.Arbitrary
-- import Control.Exception (evaluate)

import YesNoClass

spec :: Spec
spec = do

  describe "YesNo typeClass" $ do
    context "Int" $ do
      it "should return false for 0" $ do
        yesno (0 :: Int) `shouldBe` (False :: Bool)
      it "should return true for 1 and any other Int" $ do
        yesno (1 :: Int) `shouldBe` (True :: Bool)
    context "Bool" $ do
      it "should return false for False" $ do
        yesno False `shouldBe` (False :: Bool)
      it "should return true for True" $ do
        yesno True `shouldBe` (True :: Bool)
    context "List" $ do
      it "should return false for empty list []" $ do
        yesno [] `shouldBe` (False :: Bool)
      it "should return true list for [1,2,3] and any other list" $ do
        yesno [1,2,3] `shouldBe` (True :: Bool)
    context "Maybe" $ do
      it "should return false for Nothing" $ do
        yesno Nothing `shouldBe` (False :: Bool)
      it "should return true list for Just 1 and any other Just" $ do
        yesno (Just 1) `shouldBe` (True :: Bool)