-- https://www.codewars.com/kata/reverse-every-other-word-in-the-string

module ReverseEveryOtherWord (reverseEveryOther) where

import Data.List
import Data.List.Split

reverseIfOdd :: Int -> [Char] -> [Char]
reverseIfOdd i s
  | i `mod` 2 == 0 = s
  | otherwise = reverse s

reverseEveryOther :: String -> String
reverseEveryOther s = intercalate " "
  $ zipWith reverseIfOdd [0..]
  $ splitOn " " s
