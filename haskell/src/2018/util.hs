module Util where
    import Data.List (tails)

    load :: Int -> IO [String]
    load day = do
        l <- readFile ("input/day" ++ (show day) ++ ".txt")
        return (lines l)

    replace :: Char -> String -> String -> String
    replace c replacement str = concatMap (\x -> if x == c then replacement; else [x]) str

    toInt :: String -> Int
    toInt str = read str :: Int 

    allPairs :: [t] -> [(t, t)]
    allPairs a = [(x,y) | (x:ys) <- tails a, y <- ys]

    (>>>) = flip (.)