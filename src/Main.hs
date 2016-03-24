module Main where

import Data.List
import Test.Hspec


toDigits :: Integer -> [Integer]
toDigits = map (read . return) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x : 2*y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev

testCC :: IO ()
testCC = hspec $ do
  describe "Hw1" $ do
    describe "doubleEveryOther" $ do
      it "works from the back" $ do
        (doubleEveryOther [4,3,2,1]) `shouldBe` [4,6,2,2]

    describe "sumDigits" $ do
      it "sums correctly" $ do
        -- equivalent to `sum [1,6,7,1,2,5]`
        shouldBe (sumDigits [16,7,12,5]) 22

    describe "validate" $ do
      it "validates" $ do
        shouldBe (validate 4012888888881881) True
      it "invalidates" $ do
        shouldBe (validate 4012888888881882) False

-- hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

testHanoi :: IO ()
testHanoi = hspec $ do
  describe "Hanoi" $ do
    it "generates moves" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]


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

getMove :: Code -> Code -> Move
getMove = undefined

testMasterMind :: IO ()
testMasterMind = hspec $ do
  it "returns number matched" $ do
    exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] `shouldBe` 0
    exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] `shouldBe` 2

  it "can count colors" $ do
    countColors [Red, Blue, Yellow, Purple] `shouldBe` [1, 0, 1, 1, 0, 1]
    countColors [Green, Blue, Green, Orange] `shouldBe` [0, 2, 1, 0, 1, 0]

  -- it "can match" $ do
  --   matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] `shouldBe` 3

main :: IO ()
main = do
  testCC
  testHanoi
  testMasterMind
