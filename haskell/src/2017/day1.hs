module Problem where

    import Util

    main :: IO()
    main = do
        l <- load 
        print (problem l)
        print (problem2 l)

    load :: IO [String]
    load = do
        l <- readFile "input/day1.txt"
        return (lines l)

    problem :: [String] -> Int
    problem lines = mySum . toInts . cyclic . head $ lines

    problem2 :: [String] -> Int
    problem2 lines = mySum2 . pairUp . toInts . head $ lines

    cyclic :: String -> String
    cyclic line = line ++ [(head line)]

    toInts :: String -> [Int]
    toInts chars = map (\x -> read [x] :: Int) chars

    mySum :: [Int] -> Int
    mySum (i1:i2:rest)
        | i1 == i2 = i1 + mySum (i2:rest)
        | otherwise = mySum (i2:rest)
    mySum _ = 0

    -- part 2

    rotate :: [a] -> [a]
    rotate str = (drop half str) ++ (take half str)
        where half = quot (length str) 2

    pairUp :: [a] -> [(a, a)]
    pairUp str = zip str (rotate str)

    mySum2 :: [(Int, Int)] -> Int
    mySum2 ((i1,i2):rest)
        | i1 == i2 = i1 + (mySum2 rest)
        | otherwise = mySum2 rest
    mySum2 _ = 0