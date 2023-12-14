-- https://leetcode.com/problems/longest-substring-without-repeating-characters/

import Data.List

-- find the longest prefix such that all elements of the prefix are unique
uniq_prefix :: Eq a => [a] -> [a]
uniq_prefix = uniq_prefix' [] where
    uniq_prefix' p []     = p
    uniq_prefix' p (e:es) = if elem e p then p else uniq_prefix' (p++[e]) es

-- "Longest Substring Without Repeating Characters"
lsswrc :: Eq a => [a] -> Int
lsswrc = maximum . map (length . uniq_prefix) . tails

-- this version also outputs the substring it found
lsswrc' :: Eq a => [a] -> (Int, [a])
lsswrc' = foldr (\l r -> if fst l >= fst r then l else r) (0,[])
        . map (\p -> (length p, p))
        . map uniq_prefix
        . tails

