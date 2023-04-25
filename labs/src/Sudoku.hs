--Authors: Matilda Flodin & Edvin Antius

module Sudoku where
import System.Random

rows :: String
rows = "ABCD"
cols :: String
cols = "1234"

--containsElem :: Eq a => a -> [a] -> Bool
--containsElem _ [] = False
--containsElem elem (x:xs)
--    | elem == x = True
--    | otherwise = containsElem elem xs

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x, y] | x <- xs, y <- ys]

replacePointsWithZeroes :: String -> String
replacePointsWithZeroes = map aidfunc

aidfunc :: Char -> Char
aidfunc x
    |x == '.' = '0'
    |otherwise = x

--cross rows cols to get data structure
board :: [String]
board = cross rows cols

digitToInt' :: Char -> Int
digitToInt' c = read [c] :: Int

--board string
parseBoard :: String -> [(String, Int)]
parseBoard = zip board . map digitToInt' . replacePointsWithZeroes

unitList :: [[String]]
unitList = [cross [xs] cols | xs <- rows] ++ --rows
    [cross rows [xs] | xs <- cols] ++ --cols
    [cross xs ys | xs <- [take 2 rows, drop 2 rows], ys <- [take 2 cols, drop 2 cols]] --boxes


filterUnitList :: String -> [[String]]
filterUnitList st = filter (elem st) unitList

units :: [(String, [[String]])]
units = [(x, ys)| x <- board, ys <- [filterUnitList x]]

--foldList :: [[a]] -> [a]
--foldList = concat
--foldList [] = []
--foldList (x:xs) = x ++ foldList xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs


peers :: [(String, [String])]
peers = zip board $ map func $ zip board $ map (removeDuplicates . concat . snd) units
--peers = zipWith (curry func) board (map (removeDuplicates . foldList . snd) units)

func :: Eq a => (a, [a]) -> [a]
func tp = filter (/= fst tp) (snd tp)

--peers' = zip board $ map (filter (/= fst) snd) $ zip board $ map (removeDuplicates . foldList . snd) units

fromMaybe :: a -> Maybe a -> a
fromMaybe n Nothing = n
fromMaybe n (Just x) = x

--st = "A1" etc.
getPeers :: String -> [String]
getPeers st = fromMaybe ["Nothing"] $ lookup st peers

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (x:xs) = [x | Just x <- [x] ++ xs]

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups _ [] = []
lookups [] _ = []
lookups (x:xs) tl = justifyList [lookup x tl | x <- [x]++xs]

-- Ex tp = ("A1", 3), tl= board setup
validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (_, 0) tl = True
validSquare tp tl = snd tp `notElem` lookups (getPeers $ fst tp) tl

validBoard :: [(String, Int)] -> Bool
validBoard tl
    |False `elem` [validSquare s tl | s <- tl] = False
    |otherwise = True

--st = board st
verifySudoku :: String -> Bool
verifySudoku = validBoard . parseBoard

reduceList :: (Foldable t, Eq a) => [a] -> t a -> [a]
reduceList xs ys = [x | x <- xs, x `notElem` ys]

--Ex ("A1", 2) boardsetup
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, v) xs
    | (v `elem` [1..4]) && validSquare (sq, v) xs = (sq, [v])
    | (v `elem` [1..4]) && not (validSquare (sq, v) xs) = (sq, [])
    | otherwise = (sq, reduceList [1..4] (lookups (getPeers sq) xs))

--tl = board setup
validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers tl = map (`validSquareNumbers` tl) tl

--xs = units, tl = validBoardNumbers
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit xs tl
    | [] `elem` lookups xs tl = False
    | otherwise = and [x `elem` concat (lookups xs tl) | x <- [1..4]]

--tl = validboardnumbers
validUnits :: [(String, [Int])] -> Bool
validUnits tl = all (`validUnit` tl) unitList

verifySudoku' :: String -> Bool
verifySudoku' = validUnits . validBoardNumbers . parseBoard

-- "1....23...1...4." t
-- "1...2...3...4..." t
-- "1..42..33..24..1" t
-- "12341234........" f
-- "1....23.4.1...4." f

giveMeANumber :: IO ()
giveMeANumber = do
    num1 <- getLine
    num2 <- getLine
    rand <- randomRIO (read num1 :: Int, read num2 :: Int)
    print rand

chunkOf' :: Int -> [a] -> [[a]]
chunkOf' _ [] = []
chunkOf' i ls = take i ls : chunkOf' i (drop i ls)

--let tl = parseBoard "1....23.4.1...4."
printSudoku :: [(String, Int)] -> IO()
printSudoku tl = do
    let vs = [validSquare s tl | s <- tl]
    let vb = map ((not .null) . snd) (validBoardNumbers tl)
    mapM_ (putStrLn . unwords) (chunkOf' 4 $ map (show . snd) tl)
    putStr "____________"
    putStrLn "Simple conflicts: False is illegal number"
    mapM_ (putStrLn . unwords) (chunkOf' 4 $ map show vs)
    putStrLn "____________"
    putStrLn "Blocking conflicts: False squares cannot be filled"
    mapM_ (putStrLn . unwords) (chunkOf' 4 $ map show vb)
    putStrLn "____________"

