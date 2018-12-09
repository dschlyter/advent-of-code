module Util where
    import Data.List (tails, maximumBy)

    load :: Int -> IO [String]
    load day = do
        l <- readFile ("input/day" ++ (show day) ++ ".txt")
        return (lines l)

    -- strings

    replace :: Char -> String -> String -> String
    replace c replacement str = concatMap (\x -> if x == c then replacement; else [x]) str

    remove :: Char -> String -> String
    remove c str = replace c "" str

    toInt :: String -> Int
    toInt str = read str :: Int 

    split :: Char -> String -> [String]
    split token s =  case dropWhile (==token) s of
                      "" -> []
                      s' -> w : split token s''
                            where (w, s'') = break (==token) s'

    -- functors

    maxByKey :: (a -> Int) -> [a] -> a
    maxByKey key = maximumBy (\x y -> compare (key x) (key y))

    -- combinatorics

    allPairs :: [t] -> [(t, t)]
    allPairs a = [(x,y) | (x:ys) <- tails a, y <- ys]

    tuplify2 :: [a] -> (a,a)
    tuplify2 [x,y] = (x,y)
    
    tuplify3 :: [a] -> (a,a,a)
    tuplify3 [x,y,z] = (x,y,z)

    -- language

    (>>>) = flip (.)