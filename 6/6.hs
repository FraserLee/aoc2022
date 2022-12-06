-- stack script --resolver lts-20.3
import Data.List
import Data.Maybe

main = do
    file <- readFile "input.txt"
    let n = 14
    print $ (+) n $ fromJust 
          $ findIndex (((==) n) . length . nub)
          $ map (take n) 
          $ reverse $ drop (n + 1) $ reverse $ tails file

