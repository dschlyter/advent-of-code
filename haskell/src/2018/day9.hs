module Day3 where

    import Debug.Trace

    import Data.Set (Set)
    import qualified Data.Set as S
    import qualified Data.Map as M
    import Data.Function ((&))
    import Data.List (sortBy, groupBy, sort, group)
    import Data.Tuple (swap)
    import Data.Ord (compare)

    import Data.Array.ST
    import Data.Array.Unboxed

    import Util

    import Control.Monad.State

    -- input = load 9
    players = 430
    marbles = 71588

    main :: IO()
    main = do
        -- l <- input
        print (problem)
        print (problem2)

    -- cicle impl - why u no doubly linked-list haskell
    type Circle a = ([a], [a])
    circle :: a -> ([a], [a])
    circle x = ([x], [])

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

    type GameState = (Int, Circle Int)
    -- marbleGame :: GameState -> State 
    -- marbleGame

    marbleGameMonad :: State GameState Int
    marbleGameMonad = do
        (turnNumber, circle) <- get
        special <- return $ mod turnNumber 23 == 0
        put (turnNumber+1, circle & next & insert turnNumber)
        return (if special then turnNumber else 0)

    repeatMonad :: Monad m => Int -> m a -> m [a]
    repeatMonad n monad = repeat monad & take n & sequence

    -- marbleGame :: Int -> Int -> Int -> Int -> State 
    marbleGame players marbles =
        evalState (repeat marbleGameMonad & sequence) (1, circle 0)
        & take marbles
        & zip (cycle [1..players])
        & foldl (\map (player, points) -> M.insertWith (+) player points map) M.empty
        & M.toList
        & map swap
        & sort

    -- create expansion
    -- remove 
    -- mark as infinite

    mytest :: State Int String
    mytest = do
        x <- get
        put (x+1)
        return (if (x == 10) then "prize" else "nope")

    -- part 2

    problem2 :: Int
    problem2 = 2
