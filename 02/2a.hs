import System.IO
import Data.List
import Text.Read
import Data.Char

main = do
    file <- readFile "input.txt"
    print $ sum $ map score $ map (map (ord . head) . words) $ lines file
    where oA = ord 'A'
          oX = ord 'X'
          score (a:b:_) = let a' = a - oA
                              b' = b - oX
                          in b' + 1 + 3 * mod (b' - a' + 1) 3
