-- https://www.codewars.com/kata/combinations

module Combinations where

combinations :: Int -> [a] -> [[a]]
combinations 0 xs = [[]]
combinations n xs = concatMap generator (dropping xs) where
  generator [] = []
  generator (x:xs) = map (x :) $ combinations (n - 1) xs

dropping :: [a] -> [[a]]
dropping [] = [[]]
dropping (x:xs) = (x:xs) : dropping xs
