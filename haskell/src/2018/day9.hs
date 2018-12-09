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

    removeMarble :: ([a], [a]) -> ([a], [a])
    removeMarble ([], []) = ([], [])
    removeMarble (xs, []) = removeMarble (curr (xs, []))
    removeMarble (xs, y:ys) = (xs, ys)

    insert :: a -> ([a], [a]) -> ([a], [a])
    insert elem (xs, ys) = (xs, elem : ys)

    getMarble :: ([a], [a]) -> a
    getMarble c = let (x,y:ys) = (curr c) in y
    
    prev7 c = iterate prev c !! 7

    -- problem :: [String] -> Int
    problem = marbleGame players marbles

    type GameState = (Int, Circle Int)
    -- marbleGame :: GameState -> State 
    -- marbleGame

    marbleGameMonad :: State GameState Int
    marbleGameMonad = do
        (turnNumber, circle) <- get
        special <- return $ mod turnNumber 23 == 0
        put (turnNumber+1, if not special
            then circle & next & next & insert turnNumber
            else circle & prev7 & removeMarble)
        return (if special then turnNumber + (circle & prev7 & getMarble) else 0)

    -- marbleGame :: Int -> Int -> Int -> Int -> State 
    marbleGame players marbles =
        evalState (repeat marbleGameMonad & sequence) (1, circle 0)
        & take marbles
        & zip (cycle [1..players])
        & foldl (\map (player, points) -> M.insertWith (+) player points map) M.empty
        & M.toList
        & map swap
        & maximum

    -- part 2

    problem2 = marbleGame players $ marbles*100
