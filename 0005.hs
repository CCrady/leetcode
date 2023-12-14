-- https://leetcode.com/problems/longest-palindromic-substring/

import Data.List
import Data.Function

-- is the input a palindrome?
isPldrm :: Eq a => [a] -> Bool
isPldrm []  = True
isPldrm [c] = True
isPldrm str = (head str == last str) && isPldrm (init $ tail $ str)

-- list of all substrings
substrs :: [a] -> [[a]]
substrs []  = [[]]
substrs (c:cs) = map (c:) (inits cs) ++ substrs cs

-- "Longest Palindromic Substring"
lpss :: Eq a => [a] -> [a]
lpss = maximumBy (compare `on` length)
     . filter isPldrm
     . substrs

