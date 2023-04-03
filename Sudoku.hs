--Authors: Matilda Flodin & Edvin Antius

module Sudoku where
import Data.Char (digitToInt)
import Data.Maybe (isJust)

rows :: String
rows = "ABCD"
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

--board string
parseBoard :: String -> [(String, Int)]
parseBoard = zip board . map digitToInt . replacePointsWithZeroes

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

getPeers :: String -> [String]
getPeers st = fromMaybe ["Nothing"] $ lookup st peers

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (x:xs) = [x | Just x <- [x] ++ xs]

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups _ [] = []
lookups [] _ = []
lookups (x:xs) tl = justifyList [lookup x tl | x <- [x]++xs]

validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (_, 0) tl = True
validSquare tp tl = snd tp `notElem` lookups (getPeers $ fst tp) tl

