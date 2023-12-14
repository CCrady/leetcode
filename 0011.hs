-- https://leetcode.com/problems/container-with-most-water/

-- all distinct unordered pairs of elements of a list
pairs :: [a] -> [(a, a)]
pairs [x, y]  = [(x, y)]
pairs (x:xs)  = map ((,) x) xs ++ pairs xs

-- "Container With Most Water"
cwmm :: [Int] -> Int
cwmm = maximum
     . map (\((i1,h1), (i2,h2)) -> (i2 - i1) * min h1 h2)
     . pairs
     . zip [0..]

