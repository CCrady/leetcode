-- https://leetcode.com/problems/valid-number/

-- The best way to do this would be regexes, but apparently vanilla Haskell doesn't have them
import Data.List
import Data.Char

splitAtChar :: (Char -> Bool) -> String -> (String, Maybe String)
splitAtChar predicate str = let
    (pfx, sfx) = span (not . predicate) str
    exponent = if null sfx then Nothing else Just $ tail sfx
    in (pfx, exponent)

dropSign :: String -> String
dropSign str@(sign:rest) = if sign == '+' || sign == '-' then rest else str
dropSign "" = ""

isInteger :: String -> Bool
isInteger str = let rest = dropSign str in not (null rest) && all isDigit rest

isDecimal :: String -> Bool
isDecimal str = case splitAtChar (== '.') $ dropSign str of
    (_,   Nothing)  -> False -- if there's no decimal point, then it's not a decimal
    (pfx, Just sfx) -> not (null pfx && null sfx) && all isDigit pfx && all isDigit sfx


ver1 :: String -> Bool
ver1 str = case splitAtChar (\c -> c == 'e' || c == 'E') str of
    (pfx, Nothing)  -> isInteger pfx || isDecimal pfx
    (pfx, Just sfx) -> (isInteger pfx || isDecimal pfx) && isInteger sfx

