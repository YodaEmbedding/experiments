-- https://www.codewars.com/kata/find-the-parity-outlier

module Kata where

findOutlier :: [Int] -> Int
findOutlier xs = if (drop 1 evens) == [] then evens !! 0 else odds !! 0
  where
    evens = filter (\x -> x `mod` 2 == 0) xs
    odds  = filter (\x -> x `mod` 2 == 1) xs
