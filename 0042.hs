-- https://leetcode.com/problems/trapping-rain-water/

-- Algorithm: go through the shape layer-by-layer and sum the amount of water each layer can hold

import Leet (count)
import Data.List

-- we model a layer as a list of Bools, where each Bool represents whether the square is filled by
-- terrain

layerSum :: [Bool] -> Int
layerSum = count False
         . dropWhileEnd (== False)
         . dropWhile (== False)

rainWater :: [Int] -> Int
rainWater hs = let
    numLayers = maximum hs
    layers = [map (>= h) hs | h <- [1..numLayers]]
    in sum $ map layerSum layers

