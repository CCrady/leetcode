-- https://leetcode.com/problems/maximum-subarray/description/
-- TODO: figure out the divide-and-conquer solution that the problem description mentioned

maxSub = let
    f l r | l < 0     = r
          | otherwise = l + r
    in maximum . tail . scanl f 0

