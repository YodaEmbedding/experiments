import Data.List
import System.IO

-- Common functions

fib = (map fib' [0..] !!) where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n-2) + fib (n-1)

-- NOTE: slow implementation
primes :: [Integer]
primes = sieve (2 : [3, 5..]) where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

-- Problems
-- Notice that underscores denote alternate solutions (e.g. problem1_)

problem1 = sum nums where
    multiple x = x `mod` 3 == 0 || x `mod` 5 == 0
    nums = filter multiple [1..999]

problem1_ = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

problem2 = sum (takeWhile (<4000000) evens) where
    evens = filter (\n -> n `mod` 2 == 0) (map fib [2..])

problem3 = maximum (factor 600851475143) where
    divisors n = takeWhile (< (floor (sqrt n))) primes

    -- TODO passing around coll is a inefficient way to do this...
    factor n = factor' n (divisors n) [] where
        factor' :: Integer -> [Integer] -> [Integer] -> [Integer]
        factor' n (x:xs) coll
            | null xs = coll
            | n `mod` x > 0 = factor' n xs coll
            | otherwise = factor' (n `div` x) xs (coll ++ [x])

