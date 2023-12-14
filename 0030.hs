-- https://leetcode.com/problems/substring-with-concatenation-of-all-words/

import Leet (substrsOfLength, prependIf, dropIth)
import Data.List


-- All concatenations of a list of strings.
concats :: Eq a => [[a]] -> [[a]]
concats = map concat . permutations -- we could put a 'nub' in here, but that would break laziness

-- Version 1 of the solution. I think that this is less performant than version 2, but I could be
-- wrong.
ver1 :: Eq a => [[a]] -> [a] -> [Int]
ver1 wds = ver1' 0 where
    substrs = concats wds
    anyPrefixOf pfxs str = any (\e -> e `isPrefixOf` str) pfxs
    ver1' _ []  = []
    ver1' i str = anyPrefixOf substrs str `prependIf` i $ ver1' (i+1) (tail str)


-- Whether the first N characters of str are a concatenation of wds.
hasConcatPrefix :: Eq a => [[a]] -> [a] -> Bool
hasConcatPrefix []  _   = True
hasConcatPrefix _   []  = False
hasConcatPrefix wds str = let
    f (i, wd) = case stripPrefix wd str of
        Nothing   -> False
        Just rest -> hasConcatPrefix (dropIth i wds) rest
    in any f $ zip [0..] wds

-- Version 2.
ver2 :: Eq a => [[a]] -> [a] -> [Int]
ver2 wds = ver2' 0 where
    ver2' _ []  = []
    ver2' i str = hasConcatPrefix wds str `prependIf` i $ ver2' (i+1) (tail str)

