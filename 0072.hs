-- https://leetcode.com/problems/edit-distance/

-- Upper bound is abs(a-b) + min(a, b) (where a is the length of the origin string and b is the
-- length of the destination string). This is because you can always insert/remove the number of
-- chars required to get the right length (abs(a-b)) and then change all the characters that you
-- didn't insert/delete (min(a, b)).
-- This upper bound will be the right answer iff the two strings A and B have no characters in
-- common. Otherwise we could have saved some moves by not changing one or more characters during
-- the second stage.

-- Naive solution: simply do a tree search thru each possible set of edits up to the upper bound,
-- and find the minimum that way. This should ofc probably be breadth-first, so that we can short-
-- circuit on the first valid set of edits we find.

-- A better solution would likely involve some kind of substring/subarray matching. If changing a
-- character wasn't allowed, then the solution would be to find the longest shared subarray S
-- between A and B; and then the answer would be (a - s) + (b - s).

