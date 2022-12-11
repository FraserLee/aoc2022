-- stack script --resolver lts-20.3 --package split
import Data.List
import Data.List.Split
import Debug.Trace

data Monkey = Monkey {
    op :: Int -> Int,
    test :: Int -> Bool,
    passTarget :: Int,
    failTarget :: Int
}

doMonkey :: Monkey -> Int -> [[Int]] -> [[Int]]
doMonkey Monkey{op, test, passTarget, failTarget} n collection =
    let items = map (\x -> op x `div` 3) (collection !! n)
        (pass, fail) = partition test items
    in map (\(i, xs) -> case i of
                i | i == n -> []
                i | i == passTarget -> xs ++ pass
                i | i == failTarget -> xs ++ fail
                _ -> xs
           ) (zip [0..] collection)

parseItems :: String -> [Int]
parseItems = map read . splitOn ","

parseOp :: String -> Int -> Int
parseOp line = 
        let ("new":"=":a:op:b:[]) = words line
        in \x -> let a' = case a of "old" -> x ; _ -> read a
                     b' = case b of "old" -> x ; _ -> read b
                 in case op of "+" -> a' + b' ; "-" -> a' - b' ; "*" -> a' * b'

parseTest :: String -> Int -> Bool
parseTest line = let divisor = read $ last $ words line
                 in \x -> x `mod` divisor == 0

parseTarget :: String -> Int
parseTarget = read . last . words

main = do
    file <- readFile "input.txt"

    -- the important parts of each monkey section
    let raw = map (map (\x -> (splitOn ": " x) !! 1)) 
             $ splitOn [""]
             $ lines file


    -- parse the raw data into a list of item lists, list of monkeys
    let collection = map (\x -> parseItems $ x !! 1) raw
    let monkeys = map (\x -> Monkey {
        op = parseOp $ x !! 2,
        test = parseTest $ x !! 3,
        passTarget = parseTarget $ x !! 4,
        failTarget = parseTarget $ x !! 5
    }) raw

    -- fold operation count and collection of items over all monkeys, having
    -- each do its thing in turn.
    let step collection opcount = foldl (\(col, ops) (mon, n) ->
                let col' = doMonkey mon n col
                    ops' = map (\(x, i) -> 
                            if i == n 
                            then x + (length $ col !! n) 
                            else x
                        ) (zip ops [0..])
                in (col', ops')
            ) (collection, opcount) (zip monkeys [0..])

    let (_, opcount) = iterate (\(c, o) -> step c o) (collection, replicate (length collection) 0) !! 20

    -- product of the highest two opcounts
    print $ foldl (*) 1 $ take 2 $ reverse $ sort opcount

