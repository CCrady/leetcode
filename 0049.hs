-- https://leetcode.com/problems/group-anagrams/

import Data.List
import Data.Function (on)
import Data.Maybe (fromJust)

ver1 :: Ord a => [[a]] -> [[[a]]]
ver1 = map (map fst)
     . groupBy ((==) `on` snd)
     . sortBy (compare `on` snd)
     . map ((,) <*> sort)


-- ver2 only requires the lists' elements to be in Eq (not Ord), but it's less performant than ver1.
-- It builds up an ordering relation as it goes along based on the order in which it encounters
-- distinct elements.
ver2 :: Eq a => [[a]] -> [[[a]]]
ver2 = let
    -- A pair of a list specifying the ordering and an integer which is the length of the list. This
    -- is the type returned by addToOrd and updateOrd.
    --type BuiltOrdering = ([a], Int)
    -- If ord doesn't already contain x, add x to ord.
    addToOrd x ord@(xs, len) | any (== x) xs = ord
                             | otherwise     = (x:xs, len+1)
    -- Given the ordering we've built up so far and a string, return a new ordering containing any
    -- new elements that we find in the string.
    -- updateOrd: old ordering -> string -> new ordering
    updateOrd = foldr addToOrd
    -- Find the integer that x maps to under the given ordering. This will produce an error if x is
    -- not in the ordering. This is guaranteed to map x to the same value across applications, so
    -- long as the ordering is only updated additively to the start (e.g. using addToOrd or
    -- updateOrd).
    -- findInOrd: ordering -> element -> corresponding integer
    findInOrd (xs, len) x = (len -) $ fromJust $ elemIndex x xs
    -- Map each element of a string to a corresponding integer based on the given ordering, and
    -- return these integers sorted, so that (==) will act as multiset equality (i.e. unordered).
    -- numSet: ordering -> string -> integers corresponding to the elements of string
    numSet ord = sort . map (findInOrd ord)
    -- Pair each string in a list of strings with its image under numSet. Because of the properties
    -- of numSet and updateOrd, this can be done in a single pass.
    -- pairWithNumSet: list of strings -> list of (string, num set) pairs
    pairWithNumSet = pairWithNumSet' ([], 0) where
        pairWithNumSet' _   []         = []
        pairWithNumSet' ord (str:strs) = let
            newOrd = updateOrd ord str
            strNumSet = numSet newOrd str
            in (str, strNumSet) : pairWithNumSet' newOrd strs

    -- the same as ver1, but using pairWithNumSet instead of map ((,) <*> sort)
    in map (map fst)
     . groupBy ((==) `on` snd)
     . sortBy (compare `on` snd)
     . pairWithNumSet

