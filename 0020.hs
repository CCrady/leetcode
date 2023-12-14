-- https://leetcode.com/problems/valid-parentheses/

import Data.List

-- A simple short-circuit test to speed up evaluation
parens str | odd (length str) = False
parens str = parens' [] str where
    -- first argument is the stack, second is the rest of the string to process
    parens' []     []     = True
    parens' _      []     = False
    parens' []     (p:ps) = parens' [p] ps
    -- if p is an opening paren, push it into the stack
    parens' (stk)  (p:ps) | p == '(' ||
                            p == '[' ||
                            p == '{' = parens' (p:stk) (ps)
    -- if p is a closing paren, make sure it matches the top paren on the stack
    parens' (u:us) (p:ps) | u == '(' && p == ')' ||
                            u == '[' && p == ']' ||
                            u == '{' && p == '}' = parens' us ps
                          | otherwise            = False

