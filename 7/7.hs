-- stack script --resolver lts-20.3 --package regex-pcre
import Data.List
import Data.Maybe
import Text.Regex.PCRE
import Debug.Trace

data Dir = Dir { files :: [(String, Int)], dirs :: [(String, Dir)] }

instance Show Dir where
    show d = show' d ""
        where show' (Dir fs ds) prefix = unlines 
                $ map (\(name, size) -> prefix ++ "file-" ++ name ++ "-" ++ show size) fs
               ++ map (\(name, dir) -> prefix ++ "dir-" ++ name ++ "\n" ++ show' dir (prefix ++ "  ")) ds

data Action = CD String | Create String
instance Show Action where
    show (CD s) = "CD " ++ s
    show (Create s) = "Create " ++ s

pop :: Eq a => a -> [(a, b)] -> (b, [(a, b)])
pop _ [] = error "key not found"
pop key ((c_key, c_val):cs) = case key == c_key of
    True -> (c_val, cs)
    False -> let (val, cs') = pop key cs in (val, (c_key, c_val):cs')

-- act should still return the root dir, even after a cd
act :: Dir -> [Action] -> (Dir, [Action])
act dir [] = (dir, [])
act dir (CD name:as) =
    if name == ".." then (dir, as)
    else let (sub_d, dirs') = pop name (dirs dir)
             (sub_d', as') = act sub_d as
         in act (Dir (files dir) ((name, sub_d'):dirs')) as'

act dir (Create l:as) = act (add dir l) as


add :: Dir -> String -> Dir
add dir line = case line of
    ('d':'i':'r':' ':sub_d) -> 
        dir { dirs = (sub_d, Dir [] []):dirs dir }
    _ -> 
        let size':name:_ = words line 
            size = read size' :: Int
        in dir { files = (name, size):files dir }

parse_cmd :: [String] -> [Action]
parse_cmd [] = []
parse_cmd (l:lines) = case l of
    "$ ls" -> parse_ls_output lines
    _ -> CD (last $ words l) : parse_cmd lines

parse_ls_output :: [String] -> [Action]
parse_ls_output [] = []
parse_ls_output (l:lines) = case l of
    '$':_ -> parse_cmd (l:lines)
    _ -> Create l : parse_ls_output lines

size :: Dir -> Int
size (Dir fs ds) = sum (map snd fs) + sum (map (size . snd) ds)

-- traverse every directory with an accumulator
fold :: (a -> Dir -> a) -> a -> Dir -> a
fold f acc (Dir fs ds) = 
    let acc' = f acc (Dir fs ds)
    in foldl' (fold f) acc' (map snd ds)

main = do
    file <- readFile "input.txt"

    let (root, _) = act (Dir [] []) $ parse_cmd $ tail $ lines file
    let total_bounded = fold (\acc d -> if size d <= 100000 then acc + size d else acc) 0 root
    let needed_space = 30000000 - (70000000 - size root)
    let min_greater = fold (\m d -> 
                -- trace (show $ m > size d) $ trace (show $ size d >= needed_space) $ 
                if (m > size d) && (size d >= needed_space) then size d else m) 0x7fffffff root

    print $ (total_bounded, min_greater)



