-- https://stackoverflow.com/questions/52808724/the-recursion-of-custom-data-type/52808854#52808854

import Prelude

data P = P String deriving (Show,Eq,Read)
data F = F [String] deriving (Show,Eq,Read)

-- Method 1
checkout :: [P] -> [F]
checkout xs = map getname xs where
  getname (P name) = F [name]

-- Method 2
checkout :: [P] -> [F]
checkout [] = []
checkout (x:xs) = (getname x : checkout xs) where
  getname (P name) = F [name]

main :: IO ()
main = do
  putStrLn $ show $ checkout [(P "S"), (P "WW")]
