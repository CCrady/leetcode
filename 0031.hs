-- leetcode.com/problems/next-permutation/

import Data.List hiding ((!?)) -- future-proofing
import Data.Maybe
import Leet ((!?), dedup)

---- THE DUMB WAY
-- This runs in O(n!) time, since it's generating a list of n! permutations and searching through
-- them. I don't think it runs in O(1) extra space, but I don't know how to find the space
-- complexity of haskell programs.
ver1 :: Ord a => [a] -> [a]
ver1 p = let
    firstP = sort p
    maybeNextP = flip (!?) 1 $ dropWhile (/= p) $ dedup $ sort $ permutations p
    in fromMaybe firstP maybeNextP


-- "split at longest decreasing suffix"
-- Splits a list into a triple of three parts. The third is the longest decreasing suffix; the
-- second is the element immediately before the LDS; and the third is everything before the second.
-- If the entire list is decreasing, returns ([], Nothing, list).
-- O(n), n = length of list (fold w/ a constant time function)
splitAtLDS :: Ord a => [a] -> ([a], Maybe a, [a])
splitAtLDS = foldr lds' ([], Nothing, []) where
    -- O(1)
    lds' x ([], Nothing, [])   = ([], Nothing, [x])
    lds' x ([], Nothing, y:ys) | x >= y    = ([], Nothing, x:y:ys)
                               | otherwise = ([], Just x, y:ys)
    lds' x (xs, Just y,  zs)   = (x:xs, Just y, zs)

-- "replace first greater than"
-- Replaces the first element in ys that's > x with x itself. Returns the new list along with the
-- replaced element.
-- O(n), n = length of list (searching thru list in order)
replaceFstGT :: Ord a => a -> [a] -> (a, [a])
replaceFstGT x (y:ys) | y > x     = (y, x:ys)
                      | otherwise = let
                            (rplcd, ys') = replaceFstGT x ys
                            in (rplcd, y:ys')

---- THE SMARTER BUT FUGLY WAY
-- This is basically directly translated into Haskell from an imperative solution, so I dont think
-- it's very idiomatic. But it works and it has a drastically better time complexity than ver1 (O(n)
-- as opposed to O(n!)).
ver2 :: Ord a => [a] -> [a]
ver2 l = case splitAtLDS l of
    ([], Nothing, zs) -> reverse zs
    (xs, Just y,  zs) -> let
        (y', zs') = replaceFstGT y $ reverse zs
        in xs ++ y' : zs'

