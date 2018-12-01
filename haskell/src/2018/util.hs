module Util where
    replace :: Char -> String -> String -> String
    replace c replacement str = concatMap (\x -> if x == c then replacement; else [x]) str

    toInt :: String -> Int
    toInt str = read str :: Int 