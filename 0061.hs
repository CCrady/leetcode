-- https://leetcode.com/problems/rotate-list/

import Leet (eMod)
import Data.List

ver1 :: Int -> [a] -> [a]
ver1 k xs = let
    splitPos = (-k) `eMod` length xs
    (left, right) = splitAt splitPos xs
    in right ++ left

ver2 :: Int -> [a] -> [a]
ver2 k xs = let
    len = length xs
    splitPos = (-k) `eMod` len
    in take len $ drop splitPos $ cycle xs

