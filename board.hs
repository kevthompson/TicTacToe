-- don't have array of the board 
-- instead, keep an ordered list of the moves so far and reconstruct board from that
-- to check for wins, check all of following if %2 of each index is the same.
    -- The above strategy is probably terrible
-- | (1, 4, 7), (2, 5, 8), (3, 6, 9), 
-- | (1, 2, 3), (4, 5, 6), (7, 8, 9), 
-- | (1, 5, 9), (3, 5, 7)
import Data.List
import qualified Data.Set
-- get move (recursively call self until list is full)
-- getMove :: (Ord a) => [a] -> a
-- getMove n
--     | length n >= 9 = error "list got too long"
--     | length n == 8 = (read input :: Int):n
--     | otherwise     = getMove $ (read input :: Int):n
--     where input <- getLine

newBoard = [[0, 0, 0],[0, 0, 0],[0, 0, 0]]

winConditions = [(1, 4, 7), (2, 5, 8), (3, 6, 9), (1, 2, 3), (4, 5, 6), (7, 8, 9), (1, 5, 9), (3, 5, 7)]

getPiece :: Int -> Char
getPiece a
    | a == 1 = 'X'
    | a == 2 = 'O'
    | otherwise = ' '

-- printBoard a = intersperse ' ' $ intersperse '|' $ map (getPiece) $ concat $ intercalate "\n" $ map(map(show)) a
stringify :: [[Int]] -> [Char]
stringify board = intercalate "\n" $ map(show) board ++ ["\n"]
    
main = do
    putStr $ stringify newBoard
    input <- getLine
    let n = read input :: Int
    let board = takeTurn newBoard n
    putStr $ stringify board
    let winner = checkEnd board
    putStr $ declareWin winner

takeTurn :: [[Int]] -> Int -> [[Int]]
takeTurn board pos
    | pos > 8 = board
    | pos < 0 = board
    | key == 0 = modRow board pos piece
    | otherwise = newBoard
    where key = board !! (pos `div` 3) !! (pos `mod` 3)
          piece = if odd $ sum $ map(length. elemIndices 0) board then 1 else 2

modRow :: [[Int]] -> Int -> Int -> [[Int]]
modRow board pos val = (fst rows)++(modCol (head $ snd rows ) pos val):(tail $ snd rows)
    where rows = splitAt (pos `div` 3) board

modCol :: [Int] -> Int -> Int -> [Int]
modCol row pos val = (fst cols)++val:(tail $ snd cols)
    where cols = splitAt (pos `mod` 3) row

checkEnd :: [[Int]] -> Int
checkEnd board
    | winner > 0 = winner
    | zeros == 0 = -1
    | otherwise = 0
    where winner = checkWin board
          zeros = sum $ map(length. elemIndices 0) board 

checkWin :: [[Int]] -> Int
checkWin board
    | not $ null xWins = 1
    | not $ null yWins = 2
    | otherwise = 0
    where res = map (getVals board) winConditions
          xWins = filter (==1) res 
          yWins = filter (==8) res

getVals :: [[Int]] -> (Int, Int, Int) -> Int
getVals board tup = product [ board !! ((idx-1) `mod` 3) !! ((idx-1) `div` 3) | idx <- tupleToList tup] 

declareWin :: Int -> String
declareWin winner
    | winner > 0 = "We have a winner! Player token: " ++ [getPiece winner] ++ "\n"
    | winner == -1 = "It's a tie"
    | otherwise = []
    -- if n > 9 
    --     then print "value too large"

tupleToList (x, y, z) = [x, y, z]
