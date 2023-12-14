-- https://leetcode.com/problems/remove-duplicates-from-sorted-array/

-- remove consecutive duplicates from a list
rmdp []       = []
rmdp [a]      = [a]
rmdp (a:b:cs) = if a == b then rmdp (b:cs)
                          else a : rmdp (b:cs)

