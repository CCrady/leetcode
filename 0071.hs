-- https://leetcode.com/problems/simplify-path/

import Data.List (foldl', reverse, span, intercalate)

ver1 :: String -> String
ver1 = undirs . canonicalize . dirs

canonicalize :: [String] -> [String]
canonicalize = reverse . foldl' f [] where
    f stk ""   = stk
    f stk "."  = stk
    f stk ".." = if null stk then stk else tail stk
    f stk curr = curr : stk

dirs :: String -> [String]
dirs "" = []
dirs ('/':str) = let
    (top, rest) = span (/= '/') str
    in top : dirs rest

undirs :: [String] -> String
undirs = ('/' :) . intercalate "/"

