-- https://leetcode.com/problems/text-justification/

import Data.List

justify :: Int -> [String] -> [String]
justify n strs = let
    (thisLine, rest) = takeGreedy n strs
    in if null rest
        then leftJustifyLine n thisLine : []
        else if length thisLine == 1
        then leftJustifyLine n thisLine : justify n rest
        else justifyLine n thisLine : justify n rest

-- Justify just a single line.
justifyLine :: Int -> [String] -> String
justifyLine n strs = let
    totalStrLength = sum $ map length strs
    totalSpaces = n - totalStrLength
    numGaps = length strs - 1
    (gapSize, residue) = totalSpaces `divMod` numGaps
    in padBetween ' ' gapSize residue strs

-- Left justify just a single line. This is used for the final line.
leftJustifyLine :: Int -> [String] -> String
leftJustifyLine n strs = let
    unpadded = unwords strs
    len = length unpadded
    endingLength = n - len
    in unpadded ++ replicate endingLength ' '

-- Split the list of words into a prefix and a suffix, where the prefix is the largest possible
-- that will fit into n characters (when including spaces between the words).
takeGreedy :: Int -> [String] -> ([String], [String])
takeGreedy _ [] = ([], [])
takeGreedy n (wd:wds) = let
    len = length wd
    in if len > n
        then ([], wd:wds)
        else let
            charsLeft = n - len - 1
            (pfx', sfx') = takeGreedy charsLeft wds
            in (wd : pfx', sfx')

-- Pad the given character a certain number of times between the strings. Used to implement
-- justifyLine.
padBetween :: Char -> Int -> Int -> [String] -> String
padBetween c gapSize = padBetween' where
    padding = replicate gapSize c
    padBetween' _ [str] = str
    padBetween' residue (str:strs) = let
        padding' = if residue > 0 then c:padding else padding
        in str ++ padding' ++ padBetween' (residue-1) strs

