{- cabal:
build-depends: base, directory, mtl, transformers
-}

import Prelude
import Data.Char
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

firstNLettersLowercase :: Int -> String -> MaybeT [] Char
firstNLettersLowercase n xs = do
  c <- lift (take n xs)
  return $ toLower c

firstLetterMatches :: Char -> String -> Bool
firstLetterMatches letter (x:_) = x == letter
firstLetterMatches _ _ = False

-- Get word that starts with "x" from user
getXWordFromUser :: MaybeT IO String
getXWordFromUser = do
  s <- lift getLine
  guard $ firstLetterMatches 'x' s
  return s

main :: IO ()
main = do
  let s = "This is sparta!"
  print . runMaybeT $ firstNLettersLowercase 4 s

  putStrLn "Input a word starting with x (or... not x)"
  x <- runMaybeT getXWordFromUser
  print x
