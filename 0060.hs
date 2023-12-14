-- https://leetcode.com/problems/permutation-sequence/

import Leet (pick)
import Data.List


-- The dumb way
ver1 :: Int -> Int -> [Int]
ver1 n k = (!! (k-1)) $ sort $ permutations [1..n]


-- Return the representation of the input in the factorial base:
-- https://en.wikipedia.org/wiki/Factorial_number_system
-- Output is in little-endian format.
factorialBase :: Int -> [Int]
factorialBase = factorialBase' 1 where
    factorialBase' _ 0 = []
    factorialBase' p n = let
        (rest, curr) = n `divMod` p
        p' = p + 1
        in curr : factorialBase' p' rest

-- Given a padding element, a length, and a list, pad the list on the right with the padding element
-- such that the list's length is the given length. If the list is longer than the given length,
-- return the list unchanged.
rpad :: a -> Int -> [a] -> [a]
rpad _ 0 xs     = xs
rpad e n []     = e : rpad e (n-1) []
rpad e n (x:xs) = x : rpad e (n-1) xs

-- The smart way
-- Passes tests for all n in [1..7].
ver2 :: Int -> Int -> [Int]
ver2 n k = ver2' [1..n] choices where
    choices = reverse $ rpad 0 n $ factorialBase $ k - 1
    ver2' [] []     = []
    ver2' es (c:cs) = let
        (e', es') = pick c es
        in e' : ver2' es' cs

