-- Tutorial!

import Data.List
import System.IO

maxInt = maxBound :: Int
minInt = minBound :: Int

always5 :: Int
always5 = 5

always9 = 9 :: Int

modExample  = mod 5 4       -- Function call
modExample2 = 5 `mod` 4     -- Infix

sqrtOf9 = sqrt (fromIntegral always9)

-- List creation
list = [0..9]
listOdd = [1, 3..9]
list2d = [[4, 2, 0], [6, 6, 6]]
listFib = 1 : [1..3] ++ 5 : 8 : [13, 21, 34]
listChars = ['a'..'z']

-- List transforms
listExceptLast = init list
listTake = take 5 list
listDrop = drop 5 list
listRev = reverse list
listSort = sort list

-- List reduction
isEmpty = null list             -- False
contains3 = 3 `elem` list       -- True
sumList = sum list              -- 45
prodList = product list         -- 0

-- Element retrieval
thirdNum = list !! 2            -- Access 3rd element
firstNum = head list
lastNum = last list
maxNum = maximum list
minNum = minimum list

-- Advanced lists
listEvens = [0, 2..]            -- Infinite list
listConst = repeat 1            -- Infinite list
listCycle = repeat [0, 1]       -- Infinite list
listReplicate = replicate 4 [0, 1]
listSquares = [x * x | x <- [0..]]
listCubes = [x * x * x | x <- [0..]]
listEvenSquares = [x * x | x <- [0..], x `mod` 2 == 0]
listFizzBuzz = [x | x <- [0..100], x `mod` 3 == 0,
                                   x `mod` 5 == 0]
listZip = zip [0..] listSquares
listZipWith = zipWith (-) (drop 1 listSquares) listSquares
listFilter = filter (> 5) listEvens
listSquareCubes = filter
    (\x -> x `elem` (takeWhile (<= x) listSquares))
    listCubes
listSquaresMap = map (\x -> x * x) [0..]
listFactorial = foldl (*) 1 [2, 3, 4, 5]

-- Tuples
bobSmith = ("Bob Smith", 42)
bobName = fst bobSmith
bobAge = snd bobSmith

-- Functions
addMe :: Int -> Int -> Int
addMe x y = x + y

-- Pattern matching
whatAge :: Int -> String
whatAge 0 = "You're a baby... awww"
whatAge 16 = "You can drive!"
whatAge 18 = "You can drink! (If not driving.)"
whatAge 21 = "You can adult! (In theory.)"
whatAge x = if x > 21 then whatAge 21 else whatAge (x - 1)

getListItems :: [Int] -> String
getListItems [] = "Empty!"
getListItems (x:[]) = "Starts with " ++ show x
getListItems (x:y:[]) = "Contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "Contains " ++ show x ++ " and " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty"
getFirstItem all@(x:xs) = "Input: " ++ all ++ " First letter: " ++ [x]

-- Cases
whatMage :: Int -> String

whatMage n = case n of
    1 -> "Druid"
    2 -> "Elemental"
    3 -> "Chaos"
    _ -> "Yer a wizard, Harry!"

-- Guards
whatGrade :: Int -> Int
whatGrade age
    | age <= 6 = 0
    | age <= 18 = age - 6
    | otherwise = 12

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible"
    | avg <= 0.250 = "Average"
    | avg <= 0.280 = "Good"
    | otherwise = "Superstar"
    where avg = hits / atBats

-- Enumeration
data Color = Red
    | Green
    | Blue
    deriving Show

colorToString :: Color -> String
colorToString Red = "Red"
colorToString Green = "Green"
colorToString Blue = "Blue"

-- Custom tuple type
data Vector2 = Vector2 Double Double
    deriving Show

origin :: Vector2
origin = Vector2 0 0

getX :: Vector2 -> Double
getX (Vector2 x _) = x

-- Polymorphic type
data Shape =
    Circle Double Double Double
    | Rectangle Double Double Double Double

area :: Shape -> Double

area (Circle _ _ r) = pi * r^2
area (Rectangle x y x2 y2) = (abs $ x - x2) * (abs $ y - y2)

-- Type classes (using?)
data Person = Person {
    name :: String,
    age :: Int
} deriving(Eq, Show)

johnNash = Person "John Nash" 66
johnVonNeumann = Person { name="John von Neumann", age=66}

-- Type instances
data ShirtSize = S | M | L

instance Eq ShirtSize where
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False

instance Show ShirtSize where
    show S = "Small"
    show M = "Medium"
    show L = "Large"

-- Type classes
class MyEq a where
    shirtSizeEqual :: a -> a -> Bool

instance MyEq ShirtSize where
    shirtSizeEqual S S = True
    shirtSizeEqual M M = True
    shirtSizeEqual L L = True
    shirtSizeEqual _ _ = False

-- Recursion
squareList :: [Int] -> [Int]
squareList [] = []
squareList (x:xs) = x * x : squareList xs

areEqual :: String -> String -> Bool
areEqual [] [] = True
areEqual (x:xs) (y:ys) = x == y && areEqual xs ys
areEqual _ _ = False

-- Higher order functions
callFunc :: (Int -> Int) -> Int -> Int
callFunc f x = f x

pointlesslyComplicated = callFunc (*2) 4

getAdder :: Int -> (Int -> Int)
getAdder x y = x + y

adds3 = getAdder 3
seven = adds3 4

-- IO
main = do
    putStrLn "Hello world!"
    name <- getLine
    putStrLn $ "Hello, " ++ name

writeToFile = do
    file <- openFile "test.txt" WriteMode
    hPutStrLn file "FEAR OF THE DUCK"
    hClose file

readFromFile = do
    file <- openFile "test.txt" ReadMode
    contents <- hGetContents file
    putStr contents
    hClose file

-- Fibonacci
-- Tail recursive
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

