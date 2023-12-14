-- https://leetcode.com/problems/sqrtx/

-- Tested for all 0 <= n <= 500000
ver1 :: Int -> Int
ver1 0 = 0
ver1 n = ver1' lowerBound upperBound where
    minBits = log2 n `div` 2
    lowerBound = pow2 minBits
    upperBound = lowerBound * 2
    -- lo ** 2 <= n, hi ** 2 > n
    ver1' lo hi | hi <= lo + 1 = lo
                | otherwise = let
        curr = (lo + hi) `div` 2
        in if curr * curr > n
            then ver1' lo curr
            else ver1' curr hi

-- The floor of the base 2 logarithm of n (a.k.a. the number of bits it has minus 1)
log2 :: Int -> Int
log2 = log2' (-1) where
    log2' acc 0 = acc
    log2' acc n = log2' (acc+1) (n `div` 2)

-- 2 to the power of n. This is the inverse of log2.
pow2 :: Int -> Int
pow2 = pow2' 1 where
    pow2' acc 0 = acc
    pow2' acc n = pow2' (acc*2) (n-1)

