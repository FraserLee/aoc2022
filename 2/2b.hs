import System.IO
import Data.List
import Text.Read
import Data.Char

main = do
    file <- readFile "input.txt"
    print $ sum $ map score $ lines file
    where oA = ord 'A'
          oX = ord 'X'
          score (a:' ':s:_) = let a' = ord a - oA
                                  s' = ord s - oX in
                              3 * s' + 1 + mod (a' + s' - 1) 3
