module HW1.CC where

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

