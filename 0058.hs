-- https://leetcode.com/problems/length-of-last-word/

import Data.List

-- "Length of Last Word"
lolw :: String -> Int
lolw = length . last . words

