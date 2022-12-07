-- stack script --resolver lts-20.3 --package regex-pcre
import Data.List
import Data.Maybe

data Dir = Dir { files :: [(String, Int)], dirs :: [(String, Dir)] }
data Action = CD String | Create String

-- two mutually recursive functions, that hand control back and forth to represent context.
parse_prompt, parse_ls :: [String] -> [Action]
parse_prompt [] = []
parse_prompt ("$ ls":lines) = parse_ls lines
parse_prompt (cd:lines) = CD (last $ words cd) : parse_prompt lines

parse_ls [] = []
parse_ls lines@(('$':_):_) = parse_prompt lines
parse_ls (l:lines) = Create l : parse_ls lines

-- add file or directory to Dir
add :: Dir -> String -> Dir
add dir ('d':'i':'r':' ':name) = dir { dirs = (name, Dir [] []) : dirs dir }
add dir file = let size:name:_ = words file
               in dir { files = (name, read size) : files dir }

-- dir, list of actions -> possibly modified dir, probably empty list of remaining actions.
act :: Dir -> [Action] -> (Dir, [Action])
act dir [] = (dir, [])
act dir (Create l:as) = act (add dir l) as
act dir (CD "..":as) = (dir, as)
act dir (CD name:as) =

    let pop key ((c_key, c_val):cs) = case key == c_key of
            True -> (c_val, cs)
            False -> let (val, cs') = pop key cs in (val, (c_key, c_val):cs')

        (sub_d, dirs') = pop name (dirs dir)
        (sub_d', as') = act sub_d as

    in act (Dir (files dir) ((name, sub_d'):dirs')) as'


size :: Dir -> Int
size (Dir fs ds) = sum (map snd fs) + sum (map (size . snd) ds)

-- traverse every directory with an accumulator
fold :: (a -> Dir -> a) -> a -> Dir -> a
fold f acc (Dir fs ds) = let acc' = f acc (Dir fs ds)
                         in foldl' (fold f) acc' (map snd ds)

main = do

    file <- readFile "input.txt"

    let (root, _) = act (Dir [] []) $ parse_prompt $ tail $ lines file
    let total_bounded = fold (\acc d -> let s = size d in if s <= 100000 then acc + s else acc) 0 root
    let needed_space = 30000000 - (70000000 - size root)
    let min_greater = fold (\m d -> let s = size d in if (m > s) && (s >= needed_space) then s else m) 0x7fffffff root

    print (total_bounded, min_greater)
