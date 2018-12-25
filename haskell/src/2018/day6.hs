module Day6 where

    import Debug.Trace

    import Data.Set (Set)
    import qualified Data.Set as S
    import qualified Data.Map as M
    import Data.Function ((&))
    import Data.List (sortBy, groupBy, sort, group)
    import Data.Ord (compare)

    import Data.Array.ST
    import Data.Array.Unboxed

    import Util

    input = load 6

    maxSize = 300
    allPoints = [(x,y) | x <- [0..maxSize], y <- [0..maxSize]]

    main :: IO()
    main = do
        l <- input
        print (problem l)
        print (problem2 l)

    -- problem :: [String] -> Int
    problem lines = lines
        & map parse
        & intoMap

    type Point = (Int, Int)

    parse :: String -> Point
    parse line = remove ',' line & split ' ' & map toInt & tuplify2

    intoMap :: [Point] -> M.Map Point [Point]
    intoMap points = M.fromList $ zip points (repeat [])

    -- create expansion
    -- remove 
    -- mark as infinite

    -- part 2

    problem2 :: [String] -> Int
    problem2 lines = 2
