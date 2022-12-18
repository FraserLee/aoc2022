import System.IO
import Data.List
import Text.Read

main = do
    file <- readFile "input.txt"
    print $ fst $ foldl' max_group (0,0) $ map readMaybe (lines file) ++ [Nothing]
    where
        max_group (m, c) Nothing = (max m c, 0)
        max_group (m, c) (Just x) = (m, c + x)

