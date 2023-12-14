-- https://leetcode.com/problems/spiral-matrix-ii/

import Leet (Index, Bounds, Matrix)
import Data.Array


up :: Enum a => a -> a -> [a]
up lo hi = [lo .. hi]

down :: Enum a => a -> a -> [a]
down hi lo = [hi, pred hi .. lo]

-- A list of the outermost indices of a square matrix of the given size, going clockwise from the
-- top left.
spiralIndices :: Int -> [Index]
spiralIndices n = spiralIndices' 1 n where
    spiralIndices' lo hi | lo > hi = []
    spiralIndices' lo hi =
        [(x,lo) | x <- up lo hi          ] ++
        [(hi,y) | y <- up (lo+1) hi      ] ++
        [(x,hi) | x <- down (hi-1) lo    ] ++
        [(lo,y) | y <- down (hi-1) (lo+1)] ++
        spiralIndices' (lo+1) (hi-1)

spiralMatrix :: Int -> Matrix Int
spiralMatrix n = array ((1,1), (n,n)) $ zip (spiralIndices n) [1..]

