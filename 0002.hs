-- https://leetcode.com/problems/add-two-numbers/

b = 10

add3 c x y = let s = c+x+y in (s`mod`b, s`div`b)

add = add' 0 where
    add' c []     []     = if c == 0 then [] else [c]
    add' c (x:xs) []     = add' c (x:xs) [0]
    add' c []     (y:ys) = add' c [0]    (y:ys)
    add' c (x:xs) (y:ys) = let (d,c') = add3 c x y in d : add' c' xs ys

