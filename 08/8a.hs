-- stack script --resolver lts-20.3 --package regex-pcre
import Data.List
import Data.Maybe
import Data.Char



main = do
    file <- readFile "input.txt"
    let input = map (map (\c -> ord c - ord '0')) $ lines file

    let step (acc, m) x = ((x > m):acc, max m x)
    let vis arr = reverse $ fst $ foldl step ([], -1) arr

    let left   = map vis input
    let right  = map (reverse . vis . reverse) input
    let top    = transpose $ map vis $ transpose input
    let bottom = transpose $ map (reverse . vis . reverse) $ transpose input

    let or = zipWith (zipWith (||))
    let visible = or (or (or left right) top) bottom

    print $ sum $ map (foldl (\acc x -> acc + if x then 1 else 0) 0) visible

