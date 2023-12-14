-- https://leetcode.com/problems/count-and-say/

import Data.List

lookAndSay :: [Int] -> [Int]
lookAndSay = concat
           . map (\xs -> [length xs, head xs])
           . group

nth n = concat $ map show
      $ foldr ($) [1]
      $ replicate (n-1) lookAndSay
-- bonus!
printFstN n = putStrLn $ unlines
            $ map (concat . map show)
            $ take n $ iterate lookAndSay [1]

