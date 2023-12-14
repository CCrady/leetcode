-- https://leetcode.com/problems/string-to-integer-atoi/

import Data.List
import Data.Function
import Data.Char

lstrip = dropWhile (== ' ')

splitSign :: String -> (Int, String)
splitSign ('+' : cs) = (1, cs)
splitSign ('-' : cs) = ((-1), cs)
splitSign cs         = (1, cs)

-- haskell won't let me implicitly subtract one char from another :(
chrsub = (-) `on` ord

str2int :: String -> Int
str2int = foldl (\l r -> l * 10 + r `chrsub` '0') 0
        . takeWhile (\c -> '0' <= c && c <= '9')

atoi str = let
    (sign, digstr) = splitSign $ lstrip str
    in sign * str2int digstr

