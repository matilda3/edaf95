{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe

--"C:/Users/maddi/Documents/program/haskell/edaf95/labs/src/ex2.txt"
main :: IO()
main = do
  putStr "Please enter the filepath of the sudoku file: "
  file <- getLine
  contents <- readFile file
  let trim = reverse . dropWhile (=='\n') . reverse
  let sudokus = map concat (chunkOf' 9 $ filter ('=' `notElem`) (lines $ trim contents))
  --print sudokus
  putStrLn "Option 1: Solve Sudoku"
  putStrLn "Option 2: Sudoku Walkthrough"
  putStr "Please enter 1 or 2 to choose an option: "
  choice <- getLine
  if (read choice :: Int) == 1 then mapM_ (printSolution . solveSudoku) sudokus
    else do
      let solvedBoards = map solveSudoku sudokus
      let boards = map parseBoard sudokus
      mapM_ walkthrough (zip boards solvedBoards)
      putStrLn "test"
      --print bd
    --mapM_ (putStrLn . unwords) (chunkOf' 9 $ map show (concatMap snd (fromJust bd)))

walkthrough :: (Maybe Board, Maybe Board) -> IO()
walkthrough boards = do
  let solved = snd boards
  let board = fst boards
  if board == solved then putStrLn "You solved the sudoku!" else do
    mapM_ (putStrLn . unwords) (chunkOf' 9 ([if length (snd y) == 1 then show (head $ snd y) ++ " " else fst y | y <- fromJust board]))
    putStr "Please enter a square to change: "
    square <- getLine
    putStr "Please enter a number to put in that square: "
    number <- getLine
    if isNothing (assign (read number :: Int) square (fromJust board)) then putStrLn "That number doesn't work there!" else
      print $ assign (read number :: Int) square (fromJust board)
    putStrLn "test"

cross :: [a] -> [a] -> [[a]]
cross s1 s2 = [[r, c] | r <- s1, c <- s2]

rowBoxes, colBoxes :: [String]
rowBoxes = ["ABC", "DEF", "GHI"]
colBoxes = ["123", "456", "789"]

rows, cols :: String
rows = concat rowBoxes
cols = concat colBoxes

squares :: [String]
squares = cross rows cols
unitlist :: [[String]]
unitlist = [cross rows [c] | c <- cols]
        ++ [cross [r] cols | r <- rows]
        ++ [cross rs cs | rs <- rowBoxes, cs <- colBoxes]

units :: [(String, [[String]])]
units = [(s, filter (elem s) unitlist) | s <- squares]

peers :: [(String, [String])]
peers = map (\(s, u) -> (s, delete s (foldl union [] u))) units

--type that represents the board and all possible numbers for each square
type Board = [(String, [Int])]
allDigits :: [Int]
allDigits = [1, 2, 3, 4, 5, 6, 7, 8, 9]
--the list all Digits repeated infinitely [[], [], ...]
infAllDigits = repeat allDigits
emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDigit x = assign (digitToInt x) s values
  | otherwise = fail "not a valid grid"

parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares

--takes a tuple of functions and a data tuple
--returns the tuple with the corresponding function applied
map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f1, f2) tl = (f1 $ fst tl, f2 $ snd tl)

--takes a regular function, a bool function and a list
--returns a list that applies the function if 
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf func bool [] = []
mapIf func bool (x:xs)
    |bool x = func x:mapIf func bool xs
    |otherwise = x:mapIf func bool xs

--takes 2 maybes and returns a maybe
maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr a b
  |isJust a = a
  |isJust b = b
  |otherwise = Nothing

--takes list of maybes returns first just item
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust list
  |isJust $ head list = head list
  |otherwise = firstJust $ drop 1 list

-- takes comparable a, list of tuples
--returns snd of tuple where fst is a
lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList key ((f, list):rest)
  |key == f = list
  |otherwise = lookupList key rest

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind a func = func =<< a

--attempts to replace y with y' by checking each element of the list
--fmap maps function onto a functor
tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x:xs)
  |x == y = Just (y':xs)
  |otherwise = (x:) <$> tryReplace y y' xs

recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement [] [] l1 = Just l1
recursiveReplacement [a] [b] l1 = tryReplace a b l1
recursiveReplacement (x:xs) (y:ys) l1 = tryReplace x y l1 >>= recursiveReplacement xs ys

setValue :: Int -> String -> Board -> Board
setValue val sq = mapIf (map2 (unwords . words, const [val])) (\x -> sq == fst x)

eliminateValue :: Int -> String -> Board -> Board
eliminateValue val sq = mapIf (map2 (unwords . words, delete val)) (\x -> sq == fst x)

eliminate :: Int -> String -> Board -> Maybe Board
eliminate val sq board
  |null (lookupList sq (eliminateValue val sq board)) = Nothing
  |val `notElem` lookupList sq board = Just board
  |otherwise = Just $ eliminateValue val sq board

assign :: Int -> String -> Board -> Maybe Board
assign val sq bd = assign' val sq (lookupList sq peers) (setValue val sq bd)

assign' :: Int -> String -> [String] -> Board -> Maybe Board
assign' val sq [] bd = Nothing
assign' val sq [a] bd = eliminate val a bd
assign' val sq (x:xs) bd = eliminate val x bd >>= \r -> assign' val sq xs r

solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] bd = Just bd
solveSudoku' (x:xs) bd = firstJust [assign b x bd >>= solveSudoku' xs| b <- lookupList x bd]

--"003020600900305001001806400008102900700000008006708200002609500800203009005010300"

solveSudoku :: String -> Maybe Board
solveSudoku s = solveSudoku' squares (fromJust $ parseBoard s)

chunkOf' :: Int -> [a] -> [[a]]
chunkOf' _ [] = []
chunkOf' i ls = take i ls : chunkOf' i (drop i ls)

--printSolution $ solveSudoku ""
printSolution :: Maybe Board -> IO()
printSolution bd = do
  mapM_ (putStrLn . unwords) (chunkOf' 9 $ map show (concatMap snd (fromJust bd)))
  putStrLn "-----------------"