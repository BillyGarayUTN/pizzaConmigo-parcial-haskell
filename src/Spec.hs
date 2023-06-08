module Spec where
import PdePreludat
import Library
import Test.Hspec
 
correrTests :: IO ()
correrTests = hspec $ do
 describe "Test de funcionLoca" $ do
   it "numero es Impar" $ do
     doble 2 `shouldBe` 6
   

