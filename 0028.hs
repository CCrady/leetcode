-- https://leetcode.com/problems/find-the-index-of-the-first-occurrence-in-a-string/

isPrefix p s = p == take (length p) s

strstr :: Eq a => [a] -> [a] -> Int
strstr = strstr' 0 where
    strstr' :: Eq a => Int -> [a] -> [a] -> Int
    strstr' _ _ [] = -1
    strstr' i n h  = if isPrefix n h then i else strstr' (i+1) n (tail h)

