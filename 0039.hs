-- https://leetcode.com/problems/combination-sum/

comboSum :: Int -> [Int] -> [[Int]]
comboSum t = comboSum' 0 where
    comboSum' acc _      | acc == t  = [[]]
    comboSum' acc []     | acc /= t  = []
    comboSum' acc (x:xs) | acc >  t  = []
                                       -- the case where we include x in the combination
                         | otherwise = map (x:) (comboSum' (acc+x) (x:xs))
                                       -- the case where we leave x out of the combination
                                       ++ comboSum' acc xs

