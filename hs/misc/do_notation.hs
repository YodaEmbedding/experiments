{- cabal:
build-depends: base, directory
-}

import Prelude

-- Reverse a words in a list using do notation! (Inside the List monad.)
reverseWords :: [String] -> [String]
reverseWords xs = do
  x <- xs
  return $ reverse x

-- Reverse a words in a list using (>>=) notation!
reverseWordsBind :: [String] -> [String]
reverseWordsBind xs = xs >>= (\x -> return $ reverse x)
-- Simpler notations:
-- reverseWordsBind xs = xs >>= return . reverse
-- reverseWordsBind xs = reverse <$> xs

main :: IO ()
main = do
  let s = "This is sparta!"
  print . reverseWords . words $ s
  print . reverseWordsBind . words $ s
