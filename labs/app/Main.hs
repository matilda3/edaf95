module Main where

import Sudoku
import Sudoku4

main :: IO()
main = do
    contents <- readFile "blockings.txt"
    let size = length $ head $ lines contents
    let sudokus = map concat (Sudoku.chunkOf' size $ filter ('=' `notElem`) (lines contents))
    if size == 9
        then mapM_ (Sudoku.printSudoku . Sudoku.parseBoard) sudokus
    else mapM_ (Sudoku4.printSudoku . Sudoku4.parseBoard) sudokus
    