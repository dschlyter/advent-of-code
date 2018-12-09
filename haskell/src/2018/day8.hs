module Day3 where

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

    input = load 8

    main :: IO()
    main = do
        l <- input
        print (problem l)
        print (problem2 l)

    -- problem :: [String] -> Int
    problem lines = lines
        & map parse
        & zip [1..]
        & map createArea
        & expandLoop
        & filter ((== Finite) . cardinality)
        & maxByKey (\x -> length $ points x)
        & (length . points)


    parse :: String -> (Int, Int)
    parse line = remove ',' line & split ' ' & map toInt & tuplify2

    type Point = (Int, Int)
    data Area = Area {
        idNum :: Int,
        cardinality :: Cardinality,
        points :: Set (Int, Int)
    } deriving (Show)
    data Cardinality = Finite | Infinite deriving (Eq, Show)


    -- create expansion
    -- remove 
    -- mark as infinite

    -- part 2

    problem2 :: [String] -> Int
    problem2 lines = 2
