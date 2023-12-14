-- https://leetcode.com/problems/jump-game-ii/
-- This program solves a generalized version of the problem where there may or may not be a path to
-- the end of the list. If there is no path, the functions return Nothing.

import Data.List

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe (Just x) (Just y) = Just (min x y)
minMaybe x        Nothing  = x
minMaybe Nothing  y        = y

incMaybe :: Maybe Int -> Maybe Int
incMaybe (Just x) = Just (x+1)
incMaybe Nothing  = Nothing

-- Because it's essentially performing a depth-first search, this has a way slower time complexity
-- than it needs to (I want to say O(n!) but I'm not totally sure). In C the problem can be solved
-- in O(n) time and O(n) space using a variant of Dijkstra's algorithm. Since Haskell lists don't
-- have O(1) indexing, it's possible that the best we could do in Haskell is O(n^2), but that's
-- still *way* faster than O(n!).
ver1 :: [Int] -> Maybe Int
ver1 []     = Nothing
ver1 [_]    = Just 0
ver1 (0:xs) = Nothing
ver1 (x:xs) = incMaybe $ foldr minMaybe Nothing $ map ver1 $ take x $ tails xs


-- Given two lists, combine them so that each element of the resulting list is the minimum of the
-- two corresponding elements in the two source lists. If one list runs out of elements all the rest
-- of the elements are taken from the other list, as if the first list had a bunch of "nothing"
-- elements at the end which are larger than any integer.
minEach :: Ord a => [a] -> [a] -> [a]
minEach []     ys     = ys
minEach xs     []     = xs
minEach (x:xs) (y:ys) = min x y : minEach xs ys

-- Given the maximum distance we can jump from here, the number of steps it took to get here, and
-- the rest of the frontier, return an updated frontier based on how far we can jump from here.
expandFrontier :: Int -> Int -> [Int] -> [Int]
expandFrontier maxJump currSteps frontier = minEach frontier $ replicate maxJump (currSteps+1)

-- This ought to be way faster than ver1 in the limit.
ver2 :: [Int] -> Maybe Int
ver2 = ver2' [0] where
    -- if the frontier is empty, then there's no way forward
    ver2' []     _      = Nothing
    -- if the list has a single element, then we've reached the end; return the number of steps it
    -- took to get here, i.e. the first element of the frontier
    ver2' (f:_)  [_]    = Just f
    ver2' (f:fs) (j:js) = let
        nextFrontier = expandFrontier j f fs
        in ver2' nextFrontier js

