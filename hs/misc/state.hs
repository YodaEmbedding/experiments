import Prelude
import Control.Monad.State

type GameResult = Bool
type GameState = (Int, Int)

playGame :: [Int] -> State GameState GameResult

playGame [] = do
  (a, b) <- get
  return $ 2 * a >= b

playGame (x:xs) = do
  (a, b) <- get
  if x `mod` 2 == 0
     then put (a + 1, b + 1)
     else put (a,     b + 1)
  playGame xs

main :: IO ()
main = do
  putStrLn "Input at least as many evens as odds to win!\nHardest game ever!\n"
  let input = [1, 2, 3, 4]
  let initialState = (0, 0) :: GameState
  let result = evalState (playGame input) initialState
  if result
     then putStrLn "Congrats!"
     else putStrLn "Better luck next time! :("
