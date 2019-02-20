-- https://www.codewars.com/kata/n-smallest-elements-in-original-order

module Smallest where

import Data.List
import Data.Tuple

firstNSmallest :: [Int] -> Int -> [Int]
firstNSmallest xs n = map snd
  $ sort
  $ map swap
  $ take n
  $ sort
  $ zip xs [0..]
