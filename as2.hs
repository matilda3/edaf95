--Matilda Flodin & Edvin Antius

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use =<<" #-}
module SolveSudoku where

-- by Adrian Roth

import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Bifunctor
import Text.Parsec (parse)

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

getPeers :: String -> [String]
getPeers st = lookupList st peers

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
map2 (f1, f2) = Data.Bifunctor.bimap f1 f2

--takes a regular function, a bool function and a list
--returns a list that applies the function if 
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf func bool [] = []
mapIf func bool (x:xs)
    |bool x = func x:mapIf func bool xs
    |otherwise = x:mapIf func bool xs
--mapIf func bool list = [func v | v <- filter bool list] -- keeps only the parts that are used

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
setValue val sq = mapIf (map2 ((unwords . words), (\_ -> [val]))) (\x -> sq == fst x)

eliminateValue :: Int -> String -> Board -> Board
eliminateValue val sq = mapIf (map2 ((unwords . words), (delete val))) (\x -> sq == fst x)


--idk if this was what it was supposed to do with the special cases
eliminate :: Int -> String -> Board -> Maybe Board
eliminate val sq board
  |null (lookupList sq (eliminateValue val sq board)) = Nothing
  |val `notElem` lookupList sq board = Just board
--  |length (lookupList sq (eliminateValue val sq board)) == 1 = Just board
  |otherwise = Just $ eliminateValue val sq board

--lookupList sq
assign :: Int -> String -> Board -> Maybe Board
assign val sq bd = assign' val sq (lookupList sq peers) (setValue val sq bd)
--assign val sq bd = Just $ setValue val sq bd >>= \r -> fromJust (assign' val sq (lookupList sq peers) (r))
--assign val sq bd = do Just $ setValue val sq bd; assign' val sq (lookupList sq peers) bd


--returns nothing because it tries to remove values from lists where they dont exist
assign' :: Int -> String -> [String] -> Board -> Maybe Board
assign' val sq [] bd = Nothing
assign' val sq [a] bd = eliminate val a bd
assign' val sq (x:xs) bd = eliminate val x bd >>= \r -> assign' val sq xs r
--assign' val sq (x:xs) bd = do eliminated <- eliminate val x bd; assign' val sq xs bd

solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] bd = Just bd
--solveSudoku' [a] bd
--  |isJust $ firstJust [assign b a bd| b <- lookupList a bd] = firstJust [assign b a bd| b <- lookupList a bd]
--  |otherwise = Nothing--go back up
--solveSudoku' (x:xs) bd = firstJust [assign b x bd | b <- lookupList x bd] >>= solveSudoku' xs
solveSudoku' (x:xs) bd = firstJust [assign b x bd >>= solveSudoku' xs| b <- lookupList x bd]

--"003020600900305001001806400008102900700000008006708200002609500800203009005010300"

solveSudoku :: String -> Maybe Board
solveSudoku s = solveSudoku' squares (fromJust $ parseBoard s)

validSquare :: (String, Int) -> Maybe Board -> Bool
validSquare (_, 0) tl = True
validSquare sq tl = snd sq `elem` lookupList (fst sq) (extractMaybeBoard tl)
--validSquare tp tl = snd tp `notElem` concat [lookupList x (fromMaybe [("Invalid board", [0])] tl) | x <- getPeers $ fst tp]

reduceList :: (Foldable t, Eq a) => [a] -> t a -> [a]
reduceList xs ys = [x | x <- xs, x `notElem` ys]

validSquareNumbers :: String -> Maybe Board -> (String, [Int])
validSquareNumbers sq tl = (sq, lookupList sq (extractMaybeBoard tl))

--validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
--validBoardNumbers tl = [validSquareNumbers v tl | v <- tl]

validUnit :: [String] -> Maybe Board -> Bool
validUnit xs tl
    | [] `elem` lookups xs (extractMaybeBoard tl) = False
    | otherwise = and [x `elem` concat (lookups xs (extractMaybeBoard tl)) | x <- [1..9]]

--tl = validboardnumbers
validUnits :: Maybe Board -> Bool
validUnits tl = and [validUnit a tl| a <- unitlist]

validBoard :: Maybe Board -> Bool
validBoard tl
    |81 > sum [length (lookupList (fst x) (extractMaybeBoard tl)) | x <- extractMaybeBoard tl] = False
    |otherwise = True

simpleConflicts :: String -> Bool
simpleConflicts = validBoard . parseBoard

blockings :: String -> Bool
blockings = validUnits . parseBoard

verifySudoku :: String -> Bool
verifySudoku s = simpleConflicts s && blockings s
    where x = length s

chunkOf' :: Int -> [a] -> [[a]]
chunkOf' _ [] = []
chunkOf' i ls = take i ls : chunkOf' i (drop i ls)

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups _ [] = []
lookups [] _ = []
lookups (x:xs) tl = justifyList [lookup x tl | x <- x : xs]

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (x:xs) = catMaybes (x : xs)

extractMaybeBoard :: Maybe Board -> Board
extractMaybeBoard = fromMaybe [("Invalid board", [0])]

main :: IO()
main = go
    where
        go = do
            input <- getLine
            putStrLn input
            go