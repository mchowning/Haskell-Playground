{-#OPTIONS_GHC -Wall -Werror #-}

testInput :: (Num a, Ord a) => [a]
testInput = [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8, 0]

routeFinderTest :: (Num a, Ord a) => a
routeFinderTest = routeFinder testInput

--- FIRST VERSION

--routeFinder :: (Num a, Ord a) => [a] -> a
--routeFinder ls = let tupledList = makeTuples ls
--                 in min (routeTopTotaler 0 tupledList) (routeBottomTotaler 0 tupledList)
--    where
--
--        makeTuples :: (Num a, Ord a) => [a] -> [(a, a, a)]
--        makeTuples [] = []
--        makeTuples (x:y:z:zs) = (x, y, z):(makeTuples zs)
--        makeTuples _ = error "invalid input"
--
--        routeTopTotaler :: (Num a, Ord a) => a -> [(a, a, a)] -> a
--        routeTopTotaler total [] = total
--        routeTopTotaler total ((top, _, side):cs) = min (routeTopTotaler (total + top) cs) (routeBottomTotaler (total + top + side) cs)
--
--        routeBottomTotaler :: (Num a, Ord a) => a -> [(a, a, a)] -> a
--        routeBottomTotaler total [] = total
--        routeBottomTotaler total ((_, bottom, side):cs) = min (routeTopTotaler (total + bottom + side) cs) (routeBottomTotaler (total + bottom) cs)

--- SECOND VERSION

routeFinder :: (Num a, Ord a) => [a] -> a
routeFinder ls = let tupledList = makeTuples ls
                 in min (routeTopTotaler 0 tupledList) (routeBottomTotaler 0 tupledList)
    where

        makeTuples :: (Num a, Ord a) => [a] -> [(a, a, a)]
        makeTuples [] = []
        makeTuples (x:y:z:zs) = (x, y, z):(makeTuples zs)
        makeTuples _ = error "invalid input"

        routeTotaler :: (Num a, Ord a) => a -> [(a, a, a)] -> a
        routeTotaler total ((top, bottom, side)) |

        routeTopTotaler :: (Num a, Ord a) => a -> [(a, a, a)] -> a
        routeTopTotaler total [] = total
        routeTopTotaler total ((top, _, side):cs) = min (routeTopTotaler (total + top) cs) (routeBottomTotaler (total + top + side) cs)

        routeBottomTotaler :: (Num a, Ord a) => a -> [(a, a, a)] -> a
        routeBottomTotaler total [] = total
        routeBottomTotaler total ((_, bottom, side):cs) = min (routeTopTotaler (total + bottom + side) cs) (routeBottomTotaler (total + bottom) cs)
