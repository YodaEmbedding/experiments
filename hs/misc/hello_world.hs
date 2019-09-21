-- These are all ways of saying "Hello, world!"
-- See https://wiki.haskell.org/Introduction_to_IO

-- Using (>>) explicitly to chain String -> IO () instructions
main :: IO ()
main = putStrLn "Hello, world!"
  >> putStrLn "Bonjour, monde!"
  >> putStrLn "こんにちは世界"

-- Using (>>=) explicitly to chain String -> IO () instructions
main :: IO ()
main = putStrLn "Hello, world!"
  >>= \_ -> putStrLn "Bonjour, monde!"
  >>= \_ -> putStrLn "こんにちは世界"

-- Using do syntax sugar
main :: IO ()
main = do
  putStrLn "Hello, world!"
  putStrLn "Bonjour, monde!"
  putStrLn "こんにちは世界"
