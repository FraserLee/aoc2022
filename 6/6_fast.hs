-- stack script --resolver lts-20.3 --package containers --package unordered-containers
import Data.List
import Data.Maybe
import Data.Sequence (Seq ((:<|)), (|>), fromList)
import Data.Map (Map, size, empty, update, insertWith)

data Scanner = Scanner {
    queue :: Seq Char,
    count :: Map Char Int
}

scan :: Scanner -> Char -> Scanner
scan (Scanner q c) x = 
    let (y, q') = case q of y :<| q' -> (y, q')
        c' = Data.Map.update (\y -> if y == 1 then Nothing else Just (y - 1)) y c
        q'' = q' |> x
        c'' = insertWith (+) x 1 c'
    in Scanner q'' c''

main = do
    file <- readFile "input.txt"

    let n = 14

    let initial = Scanner {
        queue = fromList $ take n file,
        count = foldl (\m c -> insertWith (+) c 1 m) empty $ take n file
    }

    print $ (+ n) $ fromJust 
          $ findIndex (\s -> n == (size $ count s))
          $ scanl scan initial 
          $ drop n file

