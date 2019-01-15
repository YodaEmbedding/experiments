-- https://www.hackerrank.com/challenges/grading/problem

round5 :: Int -> Int
round5 x
  | x < 38 = x
  | x `mod` 5 < 3 = x
  | otherwise = x + 5 - x `mod` 5

main :: IO ()
main = interact $ unlines . map (show . round5 . read) . tail . words
