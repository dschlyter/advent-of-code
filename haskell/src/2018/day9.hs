module Day9 where

    import Debug.Trace

    import Data.Set (Set)
    import qualified Data.Set as S
    import qualified Data.Map as M
    import Data.Function ((&))
    import Data.List (sortBy, groupBy, sort, group, foldl')
    import Data.Tuple (swap)
    import Data.Ord (compare)

    import Data.Array.ST
    import Data.Array.Unboxed

    import Util

    import Control.Monad.State.Strict

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

    -- part 1
    problem = marbleGame players marbles

    type GameState = (Int, Circle Int)

    marbleGameMonad :: State GameState Int
    marbleGameMonad = do
        (turnNumber, circle) <- get
        special <- return $ mod turnNumber 23 == 0
        put (turnNumber+1, if not special
            then circle & next & next & insert turnNumber
            else circle & prev7 & removeMarble)
        return (if special then turnNumber + (circle & prev7 & getMarble) else 0)

    marbleGame players marbles =
        evalState (mapM (const marbleGameMonad) [1..marbles]) (1, circle 0)
        & aggScores players

    aggScores :: Int -> [Int] -> (Int, Int)
    aggScores players scores = scores 
        & zip (cycle [1..players]) 
        & foldl' (\map (player, points) -> M.insertWith (+) player points map) M.empty 
        & M.toList 
        & map swap 
        & maximum
        
    -- for some strange reason this does not work except in the repl ¯\_(ツ)_/¯
    -- & zip (cycle [1..430]) & foldr (\(player, points) map -> M.insertWith (+) player points map) M.empty & M.toList & map swap & maximum
    -- & zip (cycle [1..430]) & foldl (\map (player, points) -> M.insertWith (+) player points map) M.empty & M.toList & map swap & maximum

    -- part 2

    marbles2 = marbles * 100
    problem2 = marbleGameWithoutMonads players marbles2

    marbleGameScores :: GameState -> [Int]
    marbleGameScores (turnNumber, circle) = let
        special = mod turnNumber 23 == 0
        nextState = (turnNumber+1, if not special
            then circle & next & next & insert turnNumber
            else circle & prev7 & removeMarble)
        score = (if special then turnNumber + (circle & prev7 & getMarble) else 0)
        in score : (marbleGameScores nextState)

    marbleGameWithoutMonads players marbles =
        marbleGameScores (1, circle 0) 
        & take marbles
        & aggScores players
