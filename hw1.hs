module Main where

toDigits :: Integer -> [Integer]
toDigits = map (read . return) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [2*x]
doubleEveryOther (x:y:zs) = 2*x : y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits = sum

validate :: Integer -> Bool
validate = (== 0) . (`div` 10) .sumDigits . doubleEveryOther . toDigitsRev

main = putStrLn "derp"
