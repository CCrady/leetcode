-- https://leetcode.com/problems/merge-two-sorted-lists/

merge as     []     = as
merge []     bs     = bs
merge (a:as) (b:bs) = if a < b then a : merge (as) (b:bs)
                               else b : merge (a:as) (bs)

