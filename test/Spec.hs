{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import System.Hardware.Robko01

main :: IO ()
main = hspec $ do
    describe "cmdString" $ do
        it "returns correct string for an example command" $ do
            cmdString 9 8 1 2 3 4 `shouldBe` ":09081200030004\r\n"
