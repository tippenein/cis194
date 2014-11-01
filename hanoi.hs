module Hanoi where

import Test.Hspec
import Test.QuickCheck

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

test :: IO ()
test = hspec $ do
  describe "Hanoi" $ do
    it "generates moves" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]

main :: IO ()
main = return ()
