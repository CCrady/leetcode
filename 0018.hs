-- https://leetcode.com/problems/4sum/

import Leet (quadruples)
import Data.List

fourSum :: Int -> [Int] -> [(Int, Int, Int, Int)]
fourSum t = nub
          . filter (\(x,y,z,w) -> x+y+z+w == t)
          . quadruples

