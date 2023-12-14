-- https://leetcode.com/problems/wildcard-matching/

import Data.List

match :: String -> String -> Bool
match ('*':ws) str    = any (match ws) $ tails str
match ('?':ws) (_:cs) = match ws cs
match (w:ws)   (c:cs) = w == c && match ws cs
match ""       ""     = True
match _        _      = False

