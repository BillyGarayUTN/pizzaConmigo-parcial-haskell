module Spec where
import PdePreludat
import Library
import Test.Hspec
 
correrTests :: IO ()
correrTests = hspec $ do
 describe "Test de funcionLoca" $ do
   it "numero es Impar" $ do
     funcionLoca 3 "hola" `shouldBe` 3
   it "palabra de mayor tamanio a numero" $ do
     funcionLoca 2 "holaMundo"  `shouldBe` 9
   it "el numero es Par o palabra de menor tamanio a numero" $ do
     funcionLoca 8 "holaM"  `shouldBe` 3

