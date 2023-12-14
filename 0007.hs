-- https://leetcode.com/problems/reverse-integer/

-- the base-10 digits of n, arranged little-endianly
int2digits :: Int -> [Int]
int2digits n | n < 10    = [n]
             | otherwise = n `mod` 10 : int2digits (n `div` 10)

-- turn a digit string produced by int2digits back into an integer
digits2int :: [Int] -> Int
digits2int = foldr (\l r -> l + 10 * r) 0

reverseDigits n = (* signum n) $ digits2int $ reverse $ int2digits $ (* signum n) $ n

