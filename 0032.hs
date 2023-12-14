-- https://leetcode.com/problems/longest-valid-parentheses/

import Leet (lastElemIndex)
import Data.List
import Data.Maybe (fromJust)

--validPrefixLength = vpl' 0 0 where
    --vpl' 0   len (')':_)    = len
    --vpl' stk len ('(':rest) = vpl' (stk+1) (len+1) rest
    --vpl' stk len (')':rest) =

validPrefixLength :: String -> Int
validPrefixLength = fromJust
                  . lastElemIndex 0
                  . takeWhile (/= -1)
                  . scanl (\stk e -> if e == '(' then stk+1 else stk-1) 0

-- "Longest Valid Parentheses"
-- This works in O(n^2) time. It's possible to solve the problem in O(n) time, but it was a huge
-- pain to do it in C, so I'll save a O(n) Haskell solution for later.
lvp :: String -> Int
lvp = maximum . map validPrefixLength . tails

