import Data.Char
import qualified Data.Set as Set

main = do

    let oa = ord 'a'
    let oA = ord 'A'
    let process l = let (x,y) = splitAt (length l `div` 2) l
                        a = ord $ Set.elemAt 0 $ Set.intersection (Set.fromList x) (Set.fromList y)
                    in if a >= oa 
                       then a - oa + 1 
                       else a - oA + 27

    file <- readFile "input.txt"

    print $ sum $ map process $ lines file    

