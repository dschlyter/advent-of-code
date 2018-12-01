module Problem where

    import Prelude as P
    import Data.Set as S

    import Util as U

    main :: IO()
    main = do
        l <- load 
        print (problem l)
        print (problem2 l)

    load :: IO [String]
    load = do
        l <- readFile "input/day1.txt"
        return (lines l)

    parse :: [String] -> [Int]
    parse = (P.map U.toInt) . (P.map removePlus) 

    problem :: [String] -> Int
    problem lines = (P.foldr (+) 0) . parse $ lines

    removePlus str = U.replace '+' "" str

    mySum :: [Int] -> Int
    mySum (i1:i2:rest)
        | i1 == i2 = i1 + mySum (i2:rest)
        | otherwise = mySum (i2:rest)
    mySum _ = 0

    -- part 2

    problem2 :: [String] -> Int
    problem2 lines = cycleFreq 0 S.empty (cycle (parse lines))

    cycleFreq :: Int -> Set Int -> [Int] -> Int
    cycleFreq freq usedFreqs changes
        | S.member freq usedFreqs = freq
        | otherwise = cycleFreq newFreq (S.insert freq usedFreqs) (tail changes)
            where newFreq = freq + (head changes)