module Main where

import Sudoku

main :: IO()
main = do
    contents <- readFile "input.txt"
    let size = length $ head $ lines contents
    let sudokus = map concat (chunkOf' size $ filter ('=' `notElem`) (lines contents))
    mapM_ (printSudoku . parseBoard) sudokus