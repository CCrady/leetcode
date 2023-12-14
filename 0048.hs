-- https://leetcode.com/problems/rotate-image/

import Data.Array
import Data.List (intercalate)

type Index = (Int, Int)
type Matrix a = Array Index a

-- Given the size of a matrix and a 2D list to take its elements from, construct the matrix. Element
-- indices are one-based.
matrix :: Index -> [[a]] -> Matrix a
matrix sz lst = array ((1,1), sz)
                      [((x,y), val) | (y, row) <- zip [1..] lst,
                                      (x, val) <- zip [1..] row]

-- Like matrix, but constructs a square matrix from an integer side length.
squareMatrix :: Int -> [[a]] -> Matrix a
squareMatrix sz = matrix (sz, sz)

-- To make interpreting matrices easier, define a function to pretty print a matrix.
showMatrix :: Show a => Matrix a -> String
showMatrix m = let
    (_, (xHi, yHi)) = bounds m
    lst = [[ m ! (x, y) | x <- [1..xHi]]
                        | y <- [1..yHi]]
    in ("[" ++) . (++ "]")
     $ intercalate "\n "
     $ map (intercalate " ")
     $ map (map show)
     $ lst

putMatrix :: Show a => Matrix a -> IO ()
putMatrix = putStrLn . showMatrix

-- Given a function f that takes matrix indices to matrix indices, permute the given matrix. f takes
-- the matrix's size as its first argument and the index of the destination as its second, and
-- returns the index of the source matrix whose value should be copied to the given index of the
-- destination. This will lazily produce an exception if the source and destination have different
-- dimensions.
permuteMatrix :: (Index -> Index -> Index) -> Matrix a -> Matrix a
permuteMatrix f m = let
    bnds = bounds m
    (_, sz) = bnds
    in ixmap bnds (f sz) m

-- Ro-, ro-, ro-tatechya matrix
rotateMatrix :: Matrix a -> Matrix a
rotateMatrix = permuteMatrix rotate where
    -- Remember that (x,y) is the index in the *destination* matrix, so to rotate clockwise we need
    -- 'rotate' to map (x,y) to its *counterclockwise* image.
    rotate (_, ySz) (x,y) = (y, ySz - x + 1)

