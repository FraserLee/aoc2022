import System.IO
import Data.List
import Text.Read

main = do
    file <- readFile "input.txt"
    print $ sum $ drop 1 $ foldl' max_groups (replicate 4 0) $ map readMaybe (lines file) ++ [Nothing] 
    where 
        max_groups xs Nothing = 0 : (drop 1 (sort xs))
        max_groups (x:xs) (Just y) = (x + y) : xs

