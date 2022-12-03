import Data.Char
import qualified Data.Set as Set

main = do

    let oa = ord 'a'
    let oA = ord 'A'
    let process ls =
         let a = ord $ Set.elemAt 0 $ foldl1 Set.intersection $ map Set.fromList ls
         in if a >= oa 
            then a - oa + 1 
            else a - oA + 27

    file <- readFile "input.txt"

    let (grouped_lines, _) = span (\x -> length x == 3) $ map (take 3) $ iterate (drop 3) $ lines file
    print $ sum $ map process grouped_lines

