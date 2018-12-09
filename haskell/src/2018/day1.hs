module Problem where

    import qualified Data.Set as S
    import Data.Function ((&))

    import Util ((>>>))
    import qualified Util as U

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
    parse = (map (removePlus >>> U.toInt))

    problem :: [String] -> Int
    problem lines = lines & parse >>> foldr (+) 0

    removePlus str = U.replace '+' "" str

    mySum :: [Int] -> Int
    mySum (i1:i2:rest)
        | i1 == i2 = i1 + mySum (i2:rest)
        | otherwise = mySum (i2:rest)
    mySum _ = 0

    -- part 2

    problem2 :: [String] -> Int
    problem2 lines = cycleFreq 0 S.empty (cycle (parse lines))

    cycleFreq :: Int -> S.Set Int -> [Int] -> Int
    cycleFreq freq usedFreqs changes
        | S.member freq usedFreqs = freq
        | otherwise = cycleFreq newFreq (S.insert freq usedFreqs) (tail changes)
            where newFreq = freq + (head changes)