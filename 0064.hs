-- https://leetcode.com/problems/minimum-path-sum/

import Leet (Index, Bounds, Matrix, matrix)
import Data.Array


type CostMatrix = Matrix Int
type MinPathMatrix = Matrix Int

fromNums :: [[Int]] -> CostMatrix
fromNums cs = let
    height = length cs
    width = length $ head cs
    in matrix (width, height) cs

-- Uses the same self-referential matrix trick from 0063
minPathMatrix :: CostMatrix -> MinPathMatrix
minPathMatrix cm = pm where
    bnds@(_, (width, height)) = bounds cm
    topLeftAssoc = ((1,1), cm ! (1,1))
    pm = array bnds $
        topLeftAssoc :
        [((x,1), val) | x <- [2..width],  let val = pm ! (x-1,1) + cm ! (x,1)] ++
        [((1,y), val) | y <- [2..height], let val = pm ! (1,y-1) + cm ! (1,y)] ++
        [((x,y), val) | y <- [2..height], x <- [2..width],
            let val = (cm ! (x,y)) + min (pm ! (x-1,y)) (pm ! (x,y-1))]

ver1 :: CostMatrix -> Int
ver1 cm = minPathMatrix cm ! (snd . bounds) cm

