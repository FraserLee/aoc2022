#!/usr/bin/env stack
-- stack script --resolver lts-20.3 --package regex-posix
{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Maybe
import Text.Regex.Posix
main = do

    let process l = 

         -- more manual extraction - swapped to regex

         -- let (r1, r2) = splitAt (fromJust $ elemIndex ',' l) l
         --     (a1, b1):(a2, b2):_ = map (\r -> 
         --           (\(a, b) -> (read a :: Int, read $ tail b :: Int)) 
         --           $ splitAt (fromJust $ elemIndex '-' r) r) 
         --        $ [r1, tail r2]

         let a1:b1:a2:b2:_ = map (read . head) ((l =~ ("[0-9]+"::String))::[[String]]) :: [Int]

         -- in ((a1 <= a2) && (b1 >= b2)) || ((a2 <= a1) && (b2 >= b1)) -- part A
         in ((a1 <= a2) && (b1 >= a2)) || ((a2 <= a1) && (b2 >= a1)) -- part B

    file <- readFile "input.txt"

    print $ length $ filter (==True) $ map process $ lines file

