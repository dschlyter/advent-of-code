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

    input = load 6

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

    createArea :: (Int,(Int,Int)) -> Area
    createArea (index,pos) = Area {idNum=index, cardinality=Finite, points=S.singleton pos}

    expandLoop :: [Area] -> [Area]
    expandLoop areas = expandLoopInner areas S.empty
    -- expandLoop areas equalPoints = map (\area -> expand area (border (points area))) areas

    expandLoopInner :: [Area] -> Set Point -> [Area]
    expandLoopInner areas equalPoints = 
        let borderAreas = trace (show (length equalPoints)) map (\a -> (a, border $ points a)) areas
            expandedEqualPoints = S.union equalPoints (equalPoints & border & S.filter (not . outOfBounds))
            newEqualPoints = S.union expandedEqualPoints (map snd borderAreas & concatMap S.toList & sort & group & filter (\x -> (length x) > 1) & map head & S.fromList)
            newAreas = map (\(area, points) -> expand area (S.difference points newEqualPoints)) borderAreas
            in if (sizeSum newAreas) > (sizeSum areas) 
                then expandLoopInner newAreas newEqualPoints
                else newAreas

    border :: Set Point -> Set Point
    border points = concatMap nearby points & S.fromList & (flip S.difference) points

    nearby :: Point -> [Point]
    nearby (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

    expand :: Area -> Set Point -> Area
    expand area newPoints = 
        let inf = (any outOfBounds newPoints)
            newValid = S.filter (not . outOfBounds) newPoints in
        area { cardinality = if inf then Infinite else (cardinality area), points = S.union (points area) newValid}

    outOfBounds :: (Int, Int) -> Bool
    outOfBounds (x,y) = x < 0 || y < 0 || x > 350 || y > 350

    sizeSum :: [Area] -> Int
    sizeSum areas = map (length . points) areas & foldr (+) 0

    -- create expansion
    -- remove 
    -- mark as infinite

    -- part 2

    problem2 :: [String] -> Int
    problem2 lines = 2
