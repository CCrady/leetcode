-- https://leetcode.com/problems/n-queens/

import Data.List (permutations, replicate, intercalate, intersperse, length)
import Data.Array
import Leet (pairs, Index, Matrix, pickEach)

type Square = (Int, Int)

-- All possible configurations of n rooks on an n x n chessboard such that no two rooks can see each
-- other. This is a superset of the problem's answer.
-- Each row and each column must contain exactly one rook, so we can enumerate all possible
-- configurations by going from left to right in the columns and listing the y position of the rook
-- in each column.
rooks :: Int -> [[Square]]
rooks n = map (zip [1..n]) $ permutations [1..n]

-- Whether two bishops on the two given squares can see each other.
bishopsConflict :: Square -> Square -> Bool
bishopsConflict (x1,y1) (x2,y2) = abs (x1-x2) == abs (y1-y2)

-- Whether a configuration of bishops is peaceable, i.e., whether no two of them can see each
-- other.
peaceableBishops :: [Square] -> Bool
peaceableBishops = not . any (uncurry bishopsConflict) . pairs

-- Whether the given bishop is peaceable with all the bishops in the list. This does not check
-- whether the bishops in the list are peaceable with each other.
peaceableBishopsWith :: Square -> [Square] -> Bool
peaceableBishopsWith bishop = not . any (bishopsConflict bishop)

-- All possible configurations of n queens on an n x n chessboard such that no two queens can see
-- each other. We find this by taking all peaceable configurations of rooks and filtering out the
-- ones which have bishops in conflict. Because the queen's field of vision is the combination of
-- that of the rook and the bishop, the set of peaceable queens will be the intersection of the
-- set of peaceable rooks and the set of peaceable bishops.
queens1 :: Int -> [[Square]]
queens1 = filter peaceableBishops . rooks

-- Version 2. This is like queens1, but more efficient. Instead of generating all possible rook
-- configurations and then filtering out the ones that are invalid bishop configurations, it does a
-- depth-first search through the rook configurations and cuts off the current branch if it's
-- unworkable.
queens2 :: Int -> [[Square]]
queens2 n = queens' [1..n] 1 [] where
    queens' :: [Int] -> Int -> [Square] -> [[Square]]
    queens' [] _ qs = [qs]
    queens' ys x qs = concat [queens' ys' (x+1) (q:qs) | (y', ys') <- pickEach ys, let q = (x,y'),
                                                         peaceableBishopsWith q qs               ]

-- Get the number of solutions for a given n using the queens2 algorithm.
numSolutions :: Int -> Int
numSolutions n = numSols' [1..n] 1 [] where
    numSols' :: [Int] -> Int -> [Square] -> Int
    numSols' [] _ _ = 1
    numSols' ys x qs = sum [numSols' ys' (x+1) (q:qs) | (y', ys') <- pickEach ys, let q = (x,y'),
                                                        peaceableBishopsWith q qs               ]

---- Extra Functions

-- Turn a list of squares into a matrix of booleans, where each entry is true iff that square is
-- in the list.
config2Board :: Int -> [Square] -> Matrix Bool
config2Board n sqs = let
    allFalse = array ((1,1), (n,n)) [((x,y), False) | x <- [1..n], y <- [1..n]]
    trueEntries = map (\pos -> (pos, True)) sqs
    in allFalse // trueEntries

-- Pretty-print the board. The first argument is the character for empty board squares, and the
-- second argument is the character for occupied ones.
showBoard :: Char -> Char -> Matrix Bool -> String
showBoard c0 c1 m = let
    (_, (xHi, yHi)) = bounds m
    list :: [[Bool]]
    list = [[ m ! (x,y) | x <- [1..xHi]]
                        | y <- [1..yHi]]
    in intercalate "\n"
     $ map (intersperse ' ')
     $ map (map (\b -> if b then c1 else c0)) list

-- Convenience function to pretty-print all solutions to the problem.
putQueens :: Int -> IO ()
putQueens n = let
    divider = '\n' : replicate (2*n-1) '=' ++ ['\n']
    configs = queens2 n
    boards = map (config2Board n) configs
    strings = map (showBoard '.' 'Q') boards
    in putStrLn $ intercalate divider strings

