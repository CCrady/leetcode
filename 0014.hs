-- https://leetcode.com/problems/longest-common-prefix/

import Data.List

lcp [] = error "Cannot find longest common prefix of the empty list"
lcp [x] = x
lcp (x:xs) = lcp' x (lcp xs) where
    lcp' :: Eq a => [a] -> [a] -> [a]
    --lcp' as bs = map fst $ takeWhile (uncurry (==)) $ zip as bs
    lcp' [] _ = []
    lcp' _ [] = []
    lcp' (a:as) (b:bs) = if a == b
                         then a : lcp' as bs
                         else []

