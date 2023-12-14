-- https://leetcode.com/problems/remove-element/

-- return the haystack sans occurrences of the needle
strip _ []     = []
strip n (h:hs) = if n == h then strip n hs
                           else h : strip n hs

