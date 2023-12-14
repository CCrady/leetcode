-- https://leetcode.com/problems/palindrome-number/

-- by converting n into a string
isPalindrome1 n = let
    s = show n
    in s == reverse s

-- with just math
isPalindrome2 :: Int -> Bool
isPalindrome2 n | n < 0 = False
isPalindrome2 n = let
    decimal n | n < 10    = [n]
              | otherwise = n `mod` 10 : decimal (n `div` 10)
    digits = decimal n
    in digits == reverse digits

