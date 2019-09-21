import Prelude
import Data.Foldable
import Data.Traversable

fizzBuzz :: Int -> String
fizzBuzz x
  | mod x 15 == 0 = "FizzBuzz"
  | mod x 3  == 0 = "Fizz"
  | mod x 5  == 0 = "Buzz"
  | otherwise     = show x

fizzBuzzLoop :: Int -> [String]
fizzBuzzLoop n = f 0 0 0 where
  f i count3 count5 = do
    let x = if count3 == 0 && count5 == 0 then "FizzBuzz"
            else if count3 == 0 then "Fizz"
            else if count5 == 0 then "Buzz"
            else show i
    let count3' = if count3 + 1 < 3 then count3 + 1 else 0
    let count5' = if count5 + 1 < 5 then count5 + 1 else 0
    if i <= n then (x : f (i+1) count3' count5') else []

main :: IO ()
main = do
  forM_ (fizzBuzzLoop 31) putStrLn
  forM_ (map fizzBuzz [32..63]) putStrLn
