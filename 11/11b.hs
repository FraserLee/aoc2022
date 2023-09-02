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
    let items = map op (collection !! n)
        (pass, fail) = partition test items
    in zipWith (\i xs -> case i of
                i | i == n -> []
                i | i == passTarget -> xs ++ pass
                i | i == failTarget -> xs ++ fail
                _ -> xs
           ) [0..] collection

parseItems :: String -> [Int]
parseItems = map read . splitOn ","

parseOp :: Int -> String -> Int -> Int
parseOp lcmM line = 
        let ["new","=",a,op,b] = words line
        in \x -> let a' = case a of "old" -> x ; _ -> read a
                     b' = case b of "old" -> x ; _ -> read b
                 in (case op of "+" -> a' + b' ; "-" -> a' - b' ; "*" -> a' * b') `mod` lcmM

parseTest :: String -> Int -> Bool
parseTest line = let divisor = read $ last $ words line
                 in \x -> x `mod` divisor == 0

parseTarget :: String -> Int
parseTarget = read . last . words

main = do
    -- file <- readFile "sample.txt"
    file <- readFile "input.txt"

    -- the important parts of each monkey section
    let raw = map (map (\x -> splitOn ": " x !! 1)) 
             $ splitOn [""]
             $ lines file


    -- parse the raw data into a list of item lists, list of monkeys
    let collection = map (\x -> parseItems $ x !! 1) raw
    let lcmM = foldl1 lcm $ map (\x -> read $ last $ words $ x !! 3) raw
    let monkeys = map (\x -> Monkey {
        op = parseOp lcmM $ x !! 2,
        test = parseTest $ x !! 3,
        passTarget = parseTarget $ x !! 4,
        failTarget = parseTarget $ x !! 5
    }) raw


    -- fold operation count and collection of items over all monkeys, having
    -- each do its thing in turn.
    let step collection opcount = foldl (\(col, ops) (mon, n) -> 
                let col' = doMonkey mon n col
                    ops' = zipWith (\x i ->
                            if i == n
                            then x + toInteger (length $ col !! n)
                            else x
                        ) ops [0..]
                in (col', ops')
            ) (collection, opcount) (zip monkeys [0..]) :: ([[Int]], [Integer])

    let initOps = replicate (length collection) 0 :: [Integer]
    let (_, opcount) = iterate (uncurry step) (collection, initOps) !! 10000

    -- product of the highest two opcounts
    print $ product $ take 2 $ reverse $ sort opcount

