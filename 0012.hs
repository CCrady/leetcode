-- https://leetcode.com/problems/integer-to-roman/

import Data.List

int2rom :: Int -> String
int2rom n = let
    -- list of (digit, exp) pairs, where exp is the power of 10 place value of the given digit,
    -- arranged big-endianly
    -- e.g. 6789 -> [(6,3), (7,2), (8,1), (9,0)]
    places :: Int -> [(Int, Int)]
    places n = reverse $ zip (digits n) [0..] where
        digits n | n < 10    = [n]
                 | otherwise = n`mod`10 : digits (n`div`10)
    -- this isn't great but the alternative is casework with take & repeat
    dig2rom 0 = []
    dig2rom 1 = [0]
    dig2rom 2 = [0, 0]
    dig2rom 3 = [0, 0, 0]
    dig2rom 4 = [0, 1]
    dig2rom 5 = [1]
    dig2rom 6 = [1, 0]
    dig2rom 7 = [1, 0, 0]
    dig2rom 8 = [1, 0, 0, 0]
    dig2rom 9 = [0, 2]
    -- convert a digit-exp pair to a string of roman numerals
    place2rom (d, e) = map (shift_rom e) $ dig2rom d where
        rom_chars = ['I', 'V', 'X', 'L', 'C', 'D', 'M']
        -- find the character e*2 places after r in rom_chars (effectively multiplying by 10^e)
        shift_rom e r = rom_chars !! (r + e*2)

    in foldr (++) "" $ map place2rom $ places n

