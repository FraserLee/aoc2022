-- stack script --resolver lts-20.3
import Data.List
import Data.Maybe

data Value = I Int | L [Value] deriving (Show, Eq)
instance Ord Value where
    compare (I a) (I b) = a `compare` b
    compare (I a) b = (L [I a]) `compare` b
    compare a (I b) = a `compare` (L [I b])
    -- next 3 happen to be the default behaviour for list comparison, but I'll make it explicit
    compare (L []) (L []) = EQ
    compare (L []) _ = LT
    compare _ (L []) = GT

    compare (L as) (L bs) = as `compare` bs

parse :: String -> Value
parse s = fst $ parseV s

parseV :: String -> (Value, String)
parseV ('[':']':s) = (L [], s)
parseV ('[':s) = let (l, s') = parseL s in (L l, s')
parseV s = let (n, s') = parseI s in (I n, s')

parseI :: String -> (Int, String)
parseI s = let (n, s') = span (\c -> (c /= ',' && c /= ']')) s in (read n, s')

parseL :: String -> ([Value], String)
parseL s = let (v, s') = parseV s in case s' of
    ',' : s'' -> let (l, s''') = parseL s'' in (v : l, s''')
    ']' : s'' -> ([v], s'')

main = do
    file <- readFile "input.txt"

    let pairs = map (\[a,b,_] -> (parse a, parse b))
              $ takeWhile (not . null) $ map (take 3) $ iterate (drop 3) 
              $ (lines file) ++ [""]

    -- PART A
    print $ sum $ map fst $ filter (\(i, (a,b)) -> a <= b) $ zip [1..] pairs

    -- PART B
    let marks = [parse "[[2]]", parse "[[6]]"]
    let packets = sort $ marks ++ concatMap (\(a,b) -> [a,b]) pairs
    print $ product $ map (\m -> 1 + (fromJust $ elemIndex m packets)) marks
