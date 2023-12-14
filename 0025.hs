-- https://leetcode.com/problems/reverse-nodes-in-k-group/

import Data.List

revKGrp k list | length list < k = list
               | otherwise = let
                   (start, end) = splitAt k list
                   in reverse start ++ revKGrp k end

