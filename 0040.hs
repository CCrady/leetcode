-- https://leetcode.com/problems/combination-sum-ii/

import Data.List

comboSum :: Int -> [Int] -> [[Int]]
comboSum t = nub . comboSum' 0 . sort where
    comboSum' acc []     | acc /= t  = []
    comboSum' acc _      | acc >  t  = []
                         | acc == t  = [[]]
                           -- the case where we include x
    comboSum' acc (x:xs) = map (x:) (comboSum' (acc+x) xs)
                           -- the case where we don't
                           ++ comboSum' acc xs

