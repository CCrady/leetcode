-- https://leetcode.com/problems/add-binary/

import Data.List (reverse)

-- Tested for all inputs up to 100
calc :: String -> String -> String
calc a b = reverse $ calc' '0' (reverse a) (reverse b) where
    calc' '0' xs [] = xs
    calc' '0' [] ys = ys
    calc' '1' xs [] = calc' '0' xs ['1']
    calc' '1' [] ys = calc' '0' ['1'] ys
    calc' carry (x:xs) (y:ys) = case numOnes carry x y of
        0 -> '0' : calc' '0' xs ys
        1 -> '1' : calc' '0' xs ys
        2 -> '0' : calc' '1' xs ys
        3 -> '1' : calc' '1' xs ys
    numOnes a b c = length $ filter (== '1') [a,b,c]

