-- stack script --resolver lts-20.3
import Data.List
import Data.Char

data Pos = Pos { x :: Int, y :: Int } deriving (Eq)
instance Num Pos where
    Pos x1 y1 + Pos x2 y2 = Pos (x1 + x2) (y1 + y2)
    Pos x1 y1 - Pos x2 y2 = Pos (x1 - x2) (y1 - y2)
    Pos x1 y1 * Pos x2 y2 = Pos (x1 * x2) (y1 * y2)
    fromInteger x = Pos (fromInteger x) (fromInteger x)
    signum (Pos x y) = Pos (signum x) (signum y)
    abs (Pos x y) = Pos (abs x) (abs y)

showHeadTail :: Pos -> Pos -> String
showHeadTail (Pos hx hy) (Pos tx ty) = unlines 
                    $ map (map (\(Pos x y) -> 
                        if x == hx && y == hy then 'H' else 
                        if x == tx && y == ty then 'T' else '.'))
                    $ reverse -- make +y go up
                    $ map (\y -> map (\x -> Pos x y) [0..5]) [0..5]

main = do
    file <- readFile "input.txt"
    let steps = concat $ map (\(dir:' ':num) -> (
                    replicate (read num) (case dir of
                        'U' -> Pos 0 1 ; 'D' -> Pos 0 (-1) ; 
                        'R' -> Pos 1 0 ; 'L' -> Pos (-1) 0 ;
                ))) $ lines file

    -- new tailpos, given old tailpos and new headpos
    let tp old_tp hp = let delta = hp - old_tp
                       in if abs (x delta) > 1 || abs (y delta) > 1 
                          then old_tp + (signum delta) 
                          else old_tp

    print $ length 
          $ nub 
          -- iterate finding the next pos-list, treating the prior as the new head.
          $ iterate (\hp -> scanl tp (Pos 0 0) $ hp) 
                    (scanl (+) (Pos 0 0) steps) 
                !! 9 -- A: 1, B: 9

    -- putStrLn $ intercalate "\n" $ map (uncurry showHeadTail) $ zip hps tps
