-- https://www.codewars.com/kata/fizz-buzz

module FizzBuzz(fizzbuzz) where

fizzbuzz :: Int -> [String]
fizzbuzz n = map fizzy [1..n]

fizzy :: Int -> String
fizzy n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3  == 0 = "Fizz"
  | n `mod` 5  == 0 = "Buzz"
  | otherwise       = show n
