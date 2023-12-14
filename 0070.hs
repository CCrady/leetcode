-- https://leetcode.com/problems/climbing-stairs/

-- Naive way of getting the nth fibonacci number
ver1 :: Integral a => a -> a
ver1 0 = 1
ver1 1 = 1
ver1 n = ver1 (n-1) + ver1 (n-2)

-- Better way that doesn't require exponential recursive calls
ver2 :: Integral a => a -> a
ver2 = ver2' 0 1 where
    ver2' _ y 0 = y
    ver2' x y i = ver2' y (x+y) (i-1)

-- Using Binet's formula
-- This works for all 1 <= n <= 45, which is the requirement in the problem statement. Eventually
-- enough floating point error accumulates to make it incorrect; the first incorrect value is when
-- n == 70.
ver3 :: Integral a => a -> a
ver3 n = round $ (phi ** n' - phi' ** n') / sqrt5 where
    n' = fromIntegral $ n + 1
    sqrt5 = sqrt 5
    phi = (1 + sqrt5) / 2
    phi' = (1 - sqrt5) / 2

