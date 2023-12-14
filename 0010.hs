-- https://leetcode.com/problems/regular-expression-matching/

import Data.List

data RegexChar = Only Char | Anything
    deriving(Show)
data RegexItem = Kleene RegexChar | Single RegexChar
    deriving(Show)
type Regex = [RegexItem]

regexChar :: Char -> RegexChar
regexChar '.' = Anything
regexChar c   = Only c

charMatch :: RegexChar -> Char -> Bool
charMatch Anything  c = True
charMatch (Only rc) c = rc == c

regex :: String -> Regex
regex ""         = []
-- multiple kleene stars in a row can be simplified to just a single kleene star
regex (c:'*':cs) = Kleene (regexChar c) : regex (dropWhile (=='*') cs)
regex (c:cs)     = Single (regexChar c) : regex cs

rxMatch :: Regex -> String -> Bool
rxMatch [] "" = True
rxMatch [] _  = False
rxMatch (Single rc : rx) (c:cs) = charMatch rc c && rxMatch rx cs
rxMatch (Kleene rc : rx) str    = let
    -- every pair (pfx, sfx) s.t. pfx ++ sfx == str
    splits = [splitAt i str | i <- [0..length str]]
    splitMatches (pfx, sfx) = all (charMatch rc) pfx && rxMatch rx sfx
    in any splitMatches splits

