module Day2 where

    import qualified Data.Set as S
    import Data.Function ((&))
    import Data.List (sortBy, groupBy)
    import Data.Ord (compare)

    import Util

    input = load 2

    main :: IO()
    main = do
        l <- input
        print (problem l)
        print (problem2 l)

    problem :: [String] -> Int
    problem lines = map group lines & (\x -> countGroups 2 x * countGroups 3 x)

    group :: Ord a => [a] -> [[a]]
    group = sortBy compare >>> groupBy (==)

    hasGroup :: (Foldable t1, Foldable t) => Int -> t1 (t a) -> Bool
    hasGroup len = any (\x -> length x == len)

    countGroups :: (Foldable t1, Foldable t) => Int -> [t1 (t a)] -> Int
    countGroups size grouped = length (filter (hasGroup size) grouped)

    -- part 2

    problem2 :: [String] -> [String]
    problem2 lines = allPairs lines & concatMap (uncurry close)

    sameChars :: String -> String -> String
    sameChars (x:xs) (y:ys) = (if x == y then [x] else "") ++ sameChars xs ys
    sameChars _ _ = ""

    close :: String -> String -> [String]
    close s1 s2 = let shared = sameChars s1 s2 in
                    if length shared == length s1 - 1 then [shared] else []
