import Control.Lens
import Data.List
import Data.Maybe
import Data.Ord
import Prelude

data PlayerType = Human | AI
  deriving Eq

data Player = X | O
  deriving (Eq, Show)

newtype Board = Board [[Maybe Player]]

instance Show Board where
  show (Board board) = intercalate "\n" $ map showRow board where
    showRow xs = "[ " ++ (intercalate " " $ map showItem xs) ++ " ]"
    showItem (Just x) = show x
    showItem Nothing = " "

newtype Coord = Coord (Int, Int)
  deriving Eq

instance Show Coord where
  show (Coord (x, y)) = "(" ++ (show x) ++ " " ++ (show y) ++ ")"

boardInit :: Board
boardInit = Board (take 3 $ repeat $ take 3 $ repeat Nothing)

boardMove :: Board -> Player -> Coord -> Board
boardMove (Board board) player (Coord (x, y)) =
  Board ((element y .~ (element x .~ (Just player) $ board !! y)) board)

boardListMoves :: Board -> [Coord]
boardListMoves (Board board) = concat $ zipWith rowMoveList [0..] board where
  rowMoveList y row =
    [Coord (x, y) | (x, item) <- (zip [0..] row), isNothing item]

checkValidMove :: Board -> Coord -> Bool
checkValidMove (Board board) (Coord (x, y)) =
  x >= 0 && x < 3 &&
  y >= 0 && y < 3 &&
  (isNothing $ (board !! y) !! x)

parseInputMove :: String -> Coord
parseInputMove input = Coord (x, y) where
  sx = [input !! 0]
  sy = [input !! 2]
  x = read sx :: Int
  y = read sy :: Int

checkPlayerWin :: Board -> Player -> Bool
checkPlayerWin (Board board) player =
  (any allEqualPlayer board) ||
  (any allEqualPlayer $ transpose board) ||
  (allEqualPlayer [board !! x !! x | x <- [0..2]]) ||
  (allEqualPlayer [board !! x !! (2 - x) | x <- [0..2]])
    where
      allEqualPlayer xs = all (maybe False ((==) player)) xs

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

boardEvaluate :: Board -> Player -> Int
boardEvaluate board player
  | checkPlayerWin board player = 1
  | checkPlayerWin board $ otherPlayer player = -1
  | otherwise = 0

evaluateMove :: Board -> Player -> Coord -> Int
evaluateMove board player move = - negamax board' player' where
  board' = boardMove board player move
  player' = otherPlayer player

negamax :: Board -> Player -> Int
negamax board player
  | moves == [] || evaluation /= 0 = evaluation
  | otherwise = maximum $ map (evaluateMove board player) moves
    where
      evaluation = boardEvaluate board player
      moves = boardListMoves board

getBestMove :: Board -> Player -> Coord
getBestMove board player = maximumBy (comparing eval) moves
  where
    eval = evaluateMove board player
    moves = boardListMoves board

getAIMove :: Board -> Player -> IO Coord
getAIMove board player = do
  putStrLn "Thinking...\n"
  return $ getBestMove board player

getHumanMove :: Board -> IO Coord
getHumanMove board = do
  putStrLn "Please input a move (for example: 0 1):"
  input <- getLine
  let move = parseInputMove input
  if checkValidMove board move
     then putStrLn "" >> return move
     else putStrLn "\nInvalid move! Please try again.\n" >> getHumanMove board

getMove :: PlayerType -> Board -> Player -> IO Coord
getMove playerType board player =
  case playerType of
    Human -> getHumanMove board
    AI -> getAIMove board player

gameNext :: Board -> Player -> PlayerType -> PlayerType -> IO ()
gameNext board player playerXType playerOType
  | checkPlayerWin board X = putStrLn "Player X wins!"
  | checkPlayerWin board O = putStrLn "Player O wins!"
  | boardListMoves board == [] = putStrLn "Game ended in draw!"
  | otherwise = playGame board player playerXType playerOType

playGame :: Board -> Player -> PlayerType -> PlayerType -> IO ()
playGame board player playerXType playerOType = do
  let playerType = if player == X then playerXType else playerOType
  move <- getMove playerType board player
  let board' = boardMove board player move
  let player' = otherPlayer player
  print board'
  putStrLn ""
  gameNext board' player' playerXType playerOType

main :: IO ()
main = do
  let board = boardInit
  print board
  putStrLn ""
  playGame board X Human AI
