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

    -- input = load 9
    players = 430
    marbles = 71588

    main :: IO()
    main = do
        -- l <- input
        print (problem)
        print (problem2)

    -- cicle impl
    -- type Circle a = ([a], [a])
    createList :: a -> ([a], [a])
    createList x = ([x], [])

    curr :: ([a], [a]) -> ([a], [a])
    curr (x, []) = ([], reverse x)
    curr (x, y) = (x, y)

    next :: ([a], [a]) -> ([a], [a])
    next ([], []) = ([], [])
    next c@(xs, []) = next (curr c)
    next (xs, y:ys) = (y:xs, ys)

    prev :: ([a], [a]) -> ([a], [a])
    prev ([], []) = ([], [])
    prev ([], ys) = prev (reverse ys, [])
    prev (x:xs, ys) = (xs, x:ys)

    removeItem :: ([a], [a]) -> ([a], [a])
    removeItem ([], []) = ([], [])
    removeItem (xs, []) = removeItem (curr (xs, []))
    removeItem (xs, y:ys) = (xs, ys)

    insert :: a -> ([a], [a]) -> ([a], [a])
    insert elem (xs, ys) = (xs, elem : ys)

    -- problem :: [String] -> Int
    problem = 1 -- marbleGame 0 players 0 marbles

    marbleGame :: Int -> Int -> Int
    marbleGame

    marbleGame :: Int -> Int -> Int -> Int -> Map Int Int
    marbleGame

    -- create expansion
    -- remove 
    -- mark as infinite

    -- part 2

    problem2 :: Int
    problem2 = 2
