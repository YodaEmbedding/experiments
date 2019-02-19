-- https://www.codewars.com/kata/sum-of-the-first-nth-term-of-series

module Codewars.Kata.NthSeries where

import Prelude
import Text.Printf

seriesSum :: Integer -> String
seriesSum n = printf "%.2f" (foldr (+) 0.0 $
  [1.0 / (fromIntegral $ 1 + 3 * x) | x <- [0..(n-1)]] :: Double)
