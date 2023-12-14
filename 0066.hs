-- https://leetcode.com/problems/plus-one/

import Data.List (reverse)


ver1 :: [Int] -> [Int]
ver1 = reverse . ver1' . reverse where
    ver1' :: [Int] -> [Int]
    ver1' (9:xs) = 0   : ver1' xs
    ver1' (x:xs) = x+1 : xs
    ver1' []     = [1]


-- version that works with any Eq Bounded Enum datatype
ver2 :: (Eq a, Enum a, Bounded a) => [a] -> [a]
ver2 = reverse . ver2' . reverse where
    ver2' (x:xs) | x == maxBound = minBound : ver2' xs
    ver2' (x:xs) = succ x : xs
    ver2' []     = [succ minBound]

