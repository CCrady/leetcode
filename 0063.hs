-- https://leetcode.com/problems/unique-paths-ii/

import Leet (Index, Bounds, Matrix, matrix)
import Data.Array

data Square = Free | Obstacle deriving (Eq)

-- Helper functions to make creating obstacle matrices easier
fromNums :: [[Int]] -> Matrix Square
fromNums ns = let
    height = length ns
    width = length $ head ns
    f 0 = Free
    f 1 = Obstacle
    qs = map (map f) ns
    in matrix (width, height) qs
fromChars :: [[Char]] -> Matrix Square
fromChars cs = let
    height = length cs
    width = length $ head cs
    f ' ' = Free
    f '.' = Obstacle
    qs = map (map f) cs
    in matrix (width, height) qs


-- Given a matrix of Squares and a matrix of numbers of paths, find the number of paths to the given
-- position.
getPathValue :: Matrix Square -> Matrix Int -> Index -> Int
getPathValue _  _  (0, _) = 0
getPathValue _  _  (_, 0) = 0
getPathValue qm nm pos
    | qm ! pos == Obstacle = 0
    | otherwise            = nm ! pos

-- Given a matrix of Squares and a matrix of numbers of paths, calculate the number of paths to the
-- given position based on the numbers of paths above and to the left.
pathsToSquare :: Matrix Square -> Matrix Int -> Index -> Int
pathsToSquare qm nm (x,y) = let
    pathValues = getPathValue qm nm
    thisSquare = qm ! (x, y)
    above = pathValues (x, y-1)
    left = pathValues (x-1, y)
    in if thisSquare == Free then above + left else 0

-- Uses a trick to recursively define a matrix in terms of itself taken from section 13.2 of A
-- Gentle Introduction to Haskell, Version 98:
-- https://www.haskell.org/tutorial/arrays.html
pathMatrix :: Matrix Square -> Matrix Int
pathMatrix qm = nm where
    bnds@(_, (width, height)) = bounds qm
    topLeftAssoc = ((1,1), 1)
    nm = array bnds $
        topLeftAssoc : [(pos, pathsToSquare qm nm pos) | x <- [1..width],
                                                         y <- [1..height],
                                                         let pos = (x, y),
                                                         pos /= (1,1)]


ver1 :: Matrix Square -> Int
ver1 qm = pathMatrix qm ! snd (bounds qm)

