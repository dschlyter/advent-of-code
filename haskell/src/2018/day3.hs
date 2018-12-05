module Day3 where

    import qualified Data.Set as S
    import Data.Function ((&))
    import Data.List (sortBy, groupBy)
    import Data.Ord (compare)

    import Data.Array.ST
    import Data.Array.Unboxed

    import Util

    input = load 3

    main :: IO()
    main = do
        l <- input
        print (problem l)
        print (problem2 l)

    data Claim = Claim {
        idNum :: Int,
        pos :: (Int, Int),
        size :: (Int, Int)
    } deriving (Show)

    -- problem :: [String] -> Int
    problem lines = (head lines) &  parseClaim

    parseClaim :: String -> Claim
    parseClaim line = let [idStr, _, posStr, sizeStr] = (split ' ' line)
                          idNum = replace '#' "" idStr & toInt
                          [p1, p2] = split ',' (replace ':' "" posStr)
                          [s1, s2] = split 'x' sizeStr in 
                            Claim {idNum = idNum, pos = (toInt p1, toInt p2), size = (toInt s1, toInt s2)}

    test1 :: UArray Int Bool
    test1 = runSTUArray $ do
        bits <- newArray (0, 100) True
        return bits

    test2 :: [Int]
    test2 = [i | (i, True) <- test1]

    -- part 2

    problem2 :: [String] -> Int
    problem2 lines = 2
