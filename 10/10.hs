-- stack script --resolver lts-20.3
import Data.List

main = do
    file <- readFile "input.txt"
    let reg = reverse $ foldl (\l@(x:xs) s -> case words s of
                "addx":v:_ -> (x+(read v)):x:l
                "noop":_ -> x:l
            ) [1] $ lines file

    -- PART A
    print $ sum $ map (\i -> (reg !! (40*i + 19)) * (40*i + 20)) [0..5]

    -- PART B
    let regDelta = zipWith (-) reg $ map (\i -> i `mod` 40) [0..]
    putStrLn $ intercalate "\n" 
             $ takeWhile (not . null) $ map (take 40) $ iterate (drop 40)
             $ map (\v -> if abs v < 2 then '#' else ' ') regDelta

