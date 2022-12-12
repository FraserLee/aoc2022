-- stack script --resolver lts-20.3 --package heap
import Data.List
import Data.Heap
import Data.Char

indexOf2D :: Eq a => a -> [[a]] -> [(Int, Int)]
indexOf2D l = indexOf2D' l 0 where
        indexOf2D' [] _ = []
        indexOf2D' (x:xs) i = (map (\j -> (i, j)) (elemIndices l x)) 
                           ++ (indexOf2D' xs (i + 1))

lookup :: (Int, Int) -> [[a]] -> Maybe a
lookup (i, j) [] = Nothing
lookup (0, j) (x:_) = if j < length x then Just (x !! j) else Nothing
lookup (i, j) (_:xs) = lookup (i - 1, j) xs


main = do

    file <- readFile "input.txt"

    let world = map (map (\c -> case c of
                            'S' -> 0
                            'E' -> 25
                            _ -> ord c - ord 'a'
                    )) $ lines file

    let starts = indexOf2D 0 world
    let start = head $ indexOf2D 'S' $ lines world
    let end = head $ indexOf2D 'E' $ lines world

    -- Memoization in a functional language seems.. beyond me. I'll come back
    -- to this in a bit. For now, python.
