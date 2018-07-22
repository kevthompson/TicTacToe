-- don't have array of the board 
-- instead, keep an ordered list of the moves so far and reconstruct board from that
-- to check for wins, check all of following if %2 of each index is the same.
    -- The above strategy is probably terrible
-- | (1, 4, 7), (2, 5, 8), (3, 6, 9), 
-- | (1, 2, 3), (4, 5, 6), (7, 8, 9), 
-- | (1, 5, 9), (3, 5, 7)
import Data.List
-- get move (recursively call self until list is full)
-- getMove :: (Ord a) => [a] -> a
-- getMove n
--     | length n >= 9 = error "list got too long"
--     | length n == 8 = (read input :: Int):n
--     | otherwise     = getMove $ (read input :: Int):n
--     where input <- getLine

newBoard = [[0, 0, 0],[0, 0, 0],[0, 0, 0]]

getPiece :: Char -> Char
getPiece a
    | a == '1' = 'X'
    | a == '2' = 'O'
    | otherwise = ' '

-- printBoard a = intersperse ' ' $ intersperse '|' $ map (getPiece) $ concat $ intercalate "\n" $ map(map(show)) a
stringify :: [[Int]] -> [Char]
stringify board = intercalate "\n" $ map(show) board ++ ["\n"]
    
main = do
    input <- getLine
    let n = read input :: Int
    putStr $ stringify newBoard
    -- if n > 9 
    --     then print "value too large"

