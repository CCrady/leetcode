-- https://leetcode.com/problems/letter-combinations-of-a-phone-number/description/

import Leet (cartProd)

letterMap = [
    ('2', "abc"),
    ('3', "def"),
    ('4', "ghi"),
    ('5', "jkl"),
    ('6', "mno"),
    ('7', "pqrs"),
    ('8', "tuv"),
    ('9', "wxyz"),
    ('0', " ")
    ]

key2ltrs :: Char -> [Char]
key2ltrs key = case lookup key letterMap of
    Just x -> x
    Nothing -> error "Invalid key!"

letterCombos :: String -> [String]
-- for some reason the leetcode problem wants the empty string to map to the empty set, so we have a
-- special case for that outside the meat of the function
letterCombos "" = []
letterCombos str = foldr f [""] str where
    -- old version:
    --f d strs = map (uncurry (:)) $ cartProd (key2ltrs d) strs
    -- more readable version:
    f d strs = [ ltr : str | ltr <- key2ltrs d, str <- strs ]


