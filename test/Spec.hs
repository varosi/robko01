{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import System.Hardware.Robko01

main :: IO ()
main = hspec $ do
    describe "cmdString" $ do
        it "returns correct strings for example commands" $ do
            cmdString 9 8 1 2 3 4         `shouldBe` ":09081200030004\r\n"
            cmdString 99 88 1 2 3333 4444 `shouldBe` ":99881233334444\r\n"
