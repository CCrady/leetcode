-- https://leetcode.com/problems/unique-paths/

factorial :: Integral a => a -> Integer
factorial n = product [1 .. toInteger n]

topBottom :: Integral a => a -> a -> (a, a)
topBottom m n = let
    x = n - 1
    y = m - 1
    top = x + y
    bottom = min x y
    in (top, bottom)

-- Naive way that calculates the entire factorial of the top
ver1 :: Integral a => a -> a -> Integer
ver1 m n = let
    (top, bottom) = m `topBottom` n
    in factorial top `div` (factorial bottom * factorial (top - bottom))

-- Better way that uses the fact that factorials cancel
ver2 :: Integral a => a -> a -> Integer
ver2 m n = let
    (top, bottom) = m `topBottom` n
    leastNumerator = top - bottom + 1
    totalNumerator = product [toInteger leastNumerator .. toInteger top]
    totalDenominator = factorial bottom
    in totalNumerator `div` totalDenominator

-- Even cleverer way which minimizes the size of the Integer being held in memory
ver3 :: Integral a => a -> a -> Integer
ver3 m n = ver3' 1 1 where
    ver3' i acc | i > bottom' = acc
    ver3' i acc = ver3' (i+1) (acc * (leastNumeratorMinusOne + i) `div` i)
    -- Supplementary constants
    (top, bottom) = m `topBottom` n
    bottom' = toInteger bottom
    leastNumeratorMinusOne = toInteger $ top - bottom

