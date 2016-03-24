module HW2.Mastermind where

import Data.List
import Test.Hspec

type Code = [Color]

data Color
  = Red
  | Green
  | Blue
  | Yellow
  | Orange
  | Purple
  deriving (Show, Eq, Enum, Ord)

exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = length $ filter (\(a, b) -> a == b) zipped
  where
    zipped = zip c1 c2

colors = [Red ..]

countColors :: Code -> [Int]
countColors codes = map howMany colors
  where
    howMany c = length $ filter (\color -> color == c) codes

matches :: Code -> Code -> Int
matches c1 c2 = undefined

data Move = Code Int Int

getMove :: Code -> Code -> Move
getMove = undefined

testMastermind :: IO ()
testMastermind = hspec $ do
  it "returns number matched" $ do
    exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] `shouldBe` 0
    exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] `shouldBe` 2

  it "can count colors" $ do
    countColors [Red, Blue, Yellow, Purple] `shouldBe` [1, 0, 1, 1, 0, 1]
    countColors [Green, Blue, Green, Orange] `shouldBe` [0, 2, 1, 0, 1, 0]

  -- it "can match" $ do
  --   matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] `shouldBe` 3

