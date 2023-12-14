-- https://leetcode.com/problems/powx-n/description/

import Data.List

-- The cheeky solution
ver1 = (**)

-- The proper solution
ver2 :: Float -> Int -> Float
ver2 x n = let
    -- [x, x**2, x**4, x**8, ...]
    squares = iterate (\y -> y*y) x
    -- Decompose n into a list of bits (each represented by a Bool). The least significant bits come
    -- first. This function only works for non-negative integers. Note that it maps 0 to the empty
    -- list, as 0 has no significant bits.
    bits 0 = []
    bits n = let
        (rest, bit) = n `divMod` 2
        in (bit == 1) : bits rest
    invertIf c = if c then (1/) else id
    in invertIf (n < 0) $ product $ zipWith (\s b -> if b then s else 1) squares $ bits $ abs n

