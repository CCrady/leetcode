-- https://leetcode.com/problems/spiral-matrix/

import Leet (Index, Bounds, Matrix, emptyBounds)
import Data.Array
import Data.List (cycle, concat)
import Data.Maybe (fromMaybe)


nudgeXLo ((xLo,yLo),hi) = ((xLo+1, yLo), hi)
nudgeYLo ((xLo,yLo),hi) = ((xLo, yLo+1), hi)
nudgeXHi (lo,(xHi,yHi)) = (lo, (xHi-1, yHi))
nudgeYHi (lo,(xHi,yHi)) = (lo, (xHi, yHi-1))

up :: Enum a => a -> a -> [a]
up lo hi = [lo .. hi]

down :: Enum a => a -> a -> [a]
down hi lo = [hi, pred hi .. lo]

subMatrix :: Bounds -> Matrix a -> Matrix a
subMatrix newBounds = ixmap newBounds id

colList :: Int -> [Int] -> Matrix a -> [a]
colList x ys m = [ m ! (x,y) | y <- ys ]

rowList :: [Int] -> Int -> Matrix a -> [a]
rowList xs y m = [ m ! (x,y) | x <- xs ]

-- Take the column/row of the matrix.
topRowRight :: Matrix a -> [a]
topRowRight m = let
    ((xLo,yLo),(xHi,_)) = bounds m
    in rowList (up xLo xHi) yLo m
rightColDown :: Matrix a -> [a]
rightColDown m = let
    ((_,yLo),(xHi,yHi)) = bounds m
    in colList xHi (up yLo yHi) m
bottomRowLeft :: Matrix a -> [a]
bottomRowLeft m = let
    ((xLo,_),(xHi,yHi)) = bounds m
    in rowList (down xHi xLo) yHi m
leftColUp :: Matrix a -> [a]
leftColUp m = let
    ((xLo,yLo),(_,yHi)) = bounds m
    in colList xLo (down yHi yLo) m
-- Chop off the column/row of the matrix given by f, where f is one of the nudge* functions.
sansEdge :: (Bounds -> Bounds) -> Matrix a -> Matrix a
sansEdge f m = subMatrix (f $ bounds m) m


-- Like mapAccumL, but works on infinite lists. It stops when the function returns Nothing.
infAccumL :: (s -> a -> Maybe (s, b)) -> s -> [a] -> [b]
infAccumL f s (x:xs) = case f s x of
        Just (s', x') -> x' : infAccumL f s' xs
        Nothing       -> []


-- Version 1 chops pieces off the matrix as it goes using subMatrix.
ver1 :: Matrix a -> [a]
ver1 m = concat $ infAccumL f m funcList where
    f m _             | emptyBounds $ bounds m = Nothing
    f m (split, sans) = Just (sans m, split m)
    funcList = cycle [(topRowRight,   sansEdge nudgeYLo),
                      (rightColDown,  sansEdge nudgeXHi),
                      (bottomRowLeft, sansEdge nudgeYHi),
                      (leftColUp,     sansEdge nudgeXLo)]


-- Version 2 keeps track of the current matrix bounds in a tuple rather than chopping the matrix up.
-- TODO: clean up variable names
ver2 :: Matrix a -> [a]
ver2 m = concat $ infAccumL f (bounds m) funcList where
    f bnds _    | emptyBounds bnds = Nothing
    f bnds func = Just $ func bnds
    --funcList :: [Bounds -> (Bounds, [a])]
    funcList = cycle [top, right, bottom, left] where
        top     b@((xLo,yLo),(xHi, _ ))  =  ( nudgeYLo b,  rowList (up xLo xHi) yLo m )
        right   b@(( _ ,yLo),(xHi,yHi))  =  ( nudgeXHi b,  colList xHi (up yLo yHi) m )
        bottom  b@((xLo, _ ),(xHi,yHi))  =  ( nudgeYHi b,  rowList (down xHi xLo) yHi m )
        left    b@((xLo,yLo),( _ ,yHi))  =  ( nudgeXLo b,  colList xLo (down yHi yLo) m )


-- A list of the indices of a matrix with the given bounds, going in a clockwise spiral from the top
-- left.
spiralIndices :: Bounds -> [Index]
spiralIndices ((xLo,yLo), (xHi,yHi))
    | xLo > xHi || yLo > yHi = []
    | yLo == yHi = [(x, yLo) | x <- up xLo xHi]
    | xLo == xHi = [(xLo, y) | y <- up yLo yHi]
    | otherwise =
        [(x, yLo) | x <- up xLo xHi          ] ++
        [(xHi, y) | y <- up (yLo+1) yHi      ] ++
        [(x, yHi) | x <- down (xHi-1) xLo    ] ++
        [(xLo, y) | y <- down (yHi-1) (yLo+1)] ++
        spiralIndices ((xLo+1, yLo+1), (xHi-1, yHi-1))

-- Version 3 uses spiralIndices. It's very similar to version 2, but probably easier to understand,
-- since it uses less code overall.
ver3 :: Matrix a -> [a]
ver3 m = map (m !) $ spiralIndices $ bounds m


-- TODO
-- Version 4 calculates the index into the matrix based on the index into the spiral ordering,
-- returning an Array instead of a list. As such it's good for if you need random access to elements
-- of the spiral ordering.
--ver4 :: Matrix a -> Array Int a
--ver4 m = ixmap (1, area) transform m where
    --(_, (w, h)) = bounds m
    --area = w*h
-- Notes:
-- # of elements in a rectangular shell with outer width = w, outer height = h, and shell thickness = r:
--  w*h - (w-2r)*(h-2r)
--  => w*h - (w*h - w*2r - h*2r + 4r^2)
--  => w*2r + h*2r - 4r^2
--  => (w + h - 2r)*2r
--
-- given w, h, and n, find the largest r s.t. w*h - (w-2r)*(h-2r) < n
-- ^ this is the function that tells us how large the shell is around the element at position n in the
--   spiral ordering
-- => w*h - n < (w-2r)*(h-2r)

