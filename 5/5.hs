-- stack script --resolver lts-20.3 --package regex-pcre
import Data.List
import Data.Maybe
import Text.Regex.PCRE
import Debug.Trace

main = do
    file <- readFile "input.txt"

    -- split into two lists, one before and one after the first blank line
    let (org, mov) = break (== "") $ lines file

    let n_stacks = div (1 + (length $ head org)) 4
    let stacks = foldl build_stack (replicate n_stacks []) $ init org
    let stacks' = foldl' move stacks $ tail mov

    print $ map last stacks'

    where
        -- take existing stacks and one line of crates, return new stacks
        build_stack stacks line = 
            let items = map ((\(_:x:_) -> x) . head) 
                            (((line ++ " ") =~ ("(\\[.\\] |    )"::String))::[[String]])
            in map (\(i, s) -> if i == ' ' then s else i : s) $ zip items stacks

        -- take existing stacks and one move instruction, return new stacks
        move stacks line =
            let _:a:b:c:_ = (head $ (line =~ ("move ([0-9]+) from ([0-9]+) to ([0-9]+)"::String))::[String])
                (n, from, to) = (read a, read b, read c) :: (Int, Int, Int)

                -- take n items from first stack
                (s1, f:s2) = splitAt (from - 1) stacks
                (rest, items) = splitAt (length f - n) f
                stacks' = s1 ++ (rest : s2)

                -- put on second stack
                (s1', t:s2') = splitAt (to - 1) stacks'

            -- in s1' ++ ((t ++ reverse items) : s2') -- part A
            in s1' ++ ((t ++ items) : s2') -- part B
