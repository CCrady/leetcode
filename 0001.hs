-- https://leetcode.com/problems/two-sum/

-- return a list, in ascending order, of the integers from 0 to n, exclusive on n
iota = let
    range lo hi | lo == hi = []
                | lo <  hi = lo : range (lo+1) hi
    in range 0

-- return the list of all pairs of elements of the input such that the second item in the pair comes
-- after the first in the input list
choose2 [] = error "choose2 from empty list"
choose2 [a] = error "choose2 from singleton list"
choose2 [a, b] = [(a, b)]
choose2 (x:xs) = map (\b -> (x, b)) xs ++ choose2 xs

-- return the first element of the list which satisfies the predicate f, wrapped in Maybe in case
-- no elements satisfy f
firstSuchThat _ [] = Nothing
firstSuchThat f (x:xs) = if f x then Just x else firstSuchThat f xs


twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum ns target = let
    indexPairs :: [(Int, Int)]
    indexPairs = choose2 $ iota $ length $ ns
    satisfiesTarget :: (Int, Int) -> Bool
    satisfiesTarget (a, b) = (ns !! a) + (ns !! b) == target
    in firstSuchThat satisfiesTarget indexPairs
