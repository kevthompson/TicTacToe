-- don't have array of the board 
-- instead, keep an ordered list of the moves so far and reconstruct board from that
-- to check for wins, check all of following if %2 of each index is the same.
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

let n = []
main = do
    input <- getLine
    if length n > 9 
        then error "list got too long"
        else if length n == 9 
            then print n
        else do 
            let n = (read input :: Int) ++ n
            main