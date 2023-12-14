-- https://leetcode.com/problems/3sum/

import Data.List

-- find all distinct unordered triples of elements of a list
triples :: [a] -> [(a, a, a)]
triples [x, y, z] = [(x, y, z)]
-- TODO: is there a better way to do this?
triples (x:xs)    = let
    pairs [y, z] = [(y, z)]
    pairs (y:ys) = map ((,) y) ys ++ pairs ys
    in map (\(y, z) -> (x, y, z)) (pairs xs) ++ triples xs

threeSum :: [Int] -> [(Int, Int, Int)]
threeSum = nub
         . filter (\(x, y, z) -> x + y + z == 0)
         . triples
         -- sorting the input guarantees that each triple will be sorted, which means we can compare
         -- triples in an unordered way using (==)
         . sort

