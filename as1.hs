--authors: Matilda Flodin & Edvin Antius
--Instructions: replace the file path in the 7th line with the file path for the chosen test file.
--Run ghc --run as1.hs

main :: IO()
main = do
    contents <- readFile "FILE PATH"
    let trim = reverse . dropWhile (=='\n') . reverse
    let size = length $ head $ lines contents
    let sudokus = map (parseBoard size . concat) (chunkOf' size $ filter ('=' `notElem`) (lines $ trim contents))
    mapM_ (printSudoku size) sudokus

chunkOf' :: Int -> [a] -> [[a]]
chunkOf' _ [] = []
chunkOf' i ls = take i ls : chunkOf' i (drop i ls)

digitToInt' :: Char -> Int
digitToInt' c = read [c] :: Int

rows4 :: String
rows4 = "ABCD"
cols4 :: String
cols4 = "1234"

rows9 :: String
rows9 = "ABCDEFGHI"
cols9 :: String
cols9 = "123456789"

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x, y] | x <- xs, y <- ys]

board :: Int -> [String]
board x
    | x == 4 = cross rows4 cols4
    | otherwise = cross rows9 cols9

parseBoard :: Int -> String -> [(String, Int)]
parseBoard x = zip (board x) . map digitToInt'

unitList4 :: [[String]]
unitList4 = [cross [xs] cols4 | xs <- rows4] ++ --rows
    [cross rows4 [xs] | xs <- cols4] ++ --cols
    [cross xs ys | xs <- [take 2 rows4, drop 2 rows4], ys <- [take 2 cols4, drop 2 cols4]] --boxes

unitList9 :: [[String]]
unitList9 = [cross [xs] cols9 | xs <- rows9] ++ --rows
    [cross rows9 [xs] | xs <- cols9] ++ --cols
    [cross xs ys | xs <- [take 3 rows9, take 3 (drop 3 rows9), drop 6 rows9], ys <- [take 3 cols9, take 3 (drop 3 cols9), drop 6 cols9]] --boxes

filterUnitList :: Int -> String -> [[String]]
filterUnitList x st
    |x == 4 = filter (elem st) unitList4
    |otherwise = filter (elem st) unitList9

units :: Int -> [(String, [[String]])]
units s = [(x, ys)| x <- board s, ys <- [filterUnitList s x]]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

peers :: Int -> [(String, [String])]
peers x = zip (board x) $ zipWith (curry func) (board x) (map (removeDuplicates . concat . snd) (units x))

func :: Eq a => (a, [a]) -> [a]
func tp = filter (/= fst tp) (snd tp)

fromMaybe :: a -> Maybe a -> a
fromMaybe n Nothing = n
fromMaybe n (Just x) = x

getPeers :: Int -> String -> [String]
getPeers x st = fromMaybe ["Nothing"] $ lookup st (peers x)

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (x:xs) = [x | Just x <- x : xs]

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups _ [] = []
lookups [] _ = []
lookups (x:xs) tl = justifyList [lookup x tl | x <- x : xs]

validSquare :: Int -> (String, Int) -> [(String, Int)] -> Bool
validSquare x (_, 0) tl = True
validSquare x tp tl = snd tp `notElem` lookups (getPeers x $ fst tp) tl

reduceList :: (Foldable t, Eq a) => [a] -> t a -> [a]
reduceList xs ys = [x | x <- xs, x `notElem` ys]

validSquareNumbers :: Int -> (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers size (sq, v) xs
    | v `elem` [1..size] && validSquare size (sq, v) xs = (sq, [v])
    | v `elem` [1..size] && not (validSquare size (sq, v) xs) = (sq, [])
    | otherwise = (sq, reduceList [1..size] (lookups (getPeers size sq) xs))

validBoardNumbers :: Int -> [(String, Int)] -> [(String, [Int])]
validBoardNumbers x tl = [validSquareNumbers x v tl | v <- tl]

validUnit :: Int -> [String] -> [(String, [Int])] -> Bool
validUnit s xs tl
    | [] `elem` lookups xs tl = False
    | otherwise = and [x `elem` concat (lookups xs tl) | x <- [1..s]]

--tl = validboardnumbers
validUnits :: Int -> [(String, [Int])] -> Bool
validUnits x tl
    | x == 4 = and [validUnit x a tl| a <- unitList4]
    | otherwise = and [validUnit x a tl| a <- unitList9]

validBoard :: Int -> [(String, Int)] -> Bool
validBoard x tl
    |False `elem` [validSquare x s tl | s <- tl] = False
    |otherwise = True

simpleConflicts :: Int -> String -> Bool
simpleConflicts x = validBoard x . parseBoard x

blockings :: Int -> String -> Bool
blockings x = validUnits x . validBoardNumbers x . parseBoard x

verifySudoku :: String -> Bool
verifySudoku s = simpleConflicts x s && blockings x s
    where x = length s


printSudoku :: Int -> [(String, Int)] -> IO()
printSudoku x tl = do
    mapM_ (putStrLn . unwords) (chunkOf' x ([(if not (validSquare x y tl) then "F" else show (snd y)) | y <- tl]))
    putStr "Valid Sudoku? "
    print (verifySudoku $ concatMap (show . snd) tl)
    putStrLn "-----------------"
