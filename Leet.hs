-- Leet.hs
-- A library of useful functions for soving leetcode problems
module Leet
( eDivMod
, eDiv
, eMod
, pairs
, dupPairs
, lazyPairs
, lazyDupPairs
, triples
, dupTriples
, quadruples
, dupQuadruples
, cartProd
, prependIf
, dedup
, dropIth
, (!?)
, substrsOfLength
, elemsIndex
, lastElemIndex
, count
, countBy
, adjacentPairs
, pick
, pickEach
, isSortedBy
, isIncreasing
, isDecreasing
, Index
, Bounds
, Matrix
, emptyBounds
, matrix
, squareMatrix
, showMatrix
, putMatrix
, permuteMatrix
) where

import Data.List (elemIndex, intercalate)
import Data.Maybe (fromMaybe)
import Data.Array

-- Like divMod, but using Euclidean division (division s.t. x % y is always >= 0).
eDivMod :: Integral a => a -> a -> (a, a)
eDivMod x y = let
    isYPos = y >= 0
    (div', mod') = x `divMod` y
    div'' = if isYPos then div' else 1+div'
    mod'' = if isYPos then mod' else y+mod'
    in (div'', mod'')

-- Like div, but using Euclidean division; see eDivMod.
eDiv :: Integral a => a -> a -> a
eDiv x y = fst $ x `eDivMod` y

-- Like mod, but using Euclidean division; see eDivMod.
eMod :: Integral a => a -> a -> a
eMod x y = snd $ x `eDivMod` y

-- This is the same fixity as div, mod, quot, rem, etc.
infixl 7 `eDivMod`, `eDiv`, `eMod`

-- The list of all distinct unordered pairs of elements of a given list.
-- This function treats each element of its input as distinct, so it won't pair an element with
-- iself, but it will pair that element with another identical element. If you don't want
-- duplicates, use List.nub on the list before passing it to pairs. The first item in the pair is
-- always the one that came earlier in the list.
pairs :: [a] -> [(a, a)]
pairs [x]    = []
pairs [x, y] = [(x, y)]
pairs (x:xs) = map ((,) x) xs ++ pairs xs

-- Like pairs, but also pairs each element with itself.
dupPairs :: [a] -> [(a, a)]
dupPairs [x]    = [(x, x)]
dupPairs (x:xs) = map ((,) x) (x:xs) ++ dupPairs xs

-- Like pairs, but maximally lazy on the input list.
lazyPairs :: [a] -> [(a, a)]
lazyPairs xs = [ (l, r) | (i, r) <- zip [0..] xs,
                          l      <- take i xs   ]

-- Like dupPairs, but maximally lazy on the input list.
lazyDupPairs :: [a] -> [(a, a)]
lazyDupPairs xs = [ (l, r) | (i, r) <- zip [1..] xs,
                             l      <- take i xs   ]

-- Like pairs, but produces triples.
triples :: [a] -> [(a, a, a)]
triples [x, y, z] = [(x, y, z)]
triples (x:xs)    = map (\(y, z) -> (x, y, z)) (pairs xs) ++ triples xs

-- Like triples, but allows more than one copy of each element in a triple.
dupTriples :: [a] -> [(a, a, a)]
dupTriples [x]    = [(x, x, x)]
dupTriples (x:xs) = map (\(y, z) -> (x, y, z)) (dupPairs (x:xs)) ++ dupTriples xs

-- Like pairs & triples, but produces quadruples.
quadruples :: [a] -> [(a, a, a, a)]
quadruples [x, y, z, w] = [(x, y, z, w)]
quadruples (x:xs)       = map (\(y, z, w) -> (x, y, z, w)) (triples (xs)) ++ quadruples xs

-- You get the idea.
dupQuadruples :: [a] -> [(a, a, a, a)]
dupQuadruples [x]    = [(x, x, x, x)]
dupQuadruples (x:xs) = map (\(y, z, w) -> (x, y, z, w)) (dupTriples (x:xs)) ++ dupQuadruples xs

-- Cartesian product of two lists.
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
-- Because (,) is a value constructor for pairs and lists are an instance of Applicative, this could
-- also be written as:
--cartProd xs ys = (,) <$> xs <*> ys

-- Prepend x to xs iff some condition is met.
prependIf :: Bool -> a -> [a] -> [a]
prependIf c x xs = if c then x : xs else xs
infix 1 `prependIf`

-- Remove adjacent duplicate entries from a list.
dedup :: Eq a => [a] -> [a]
dedup []       = []
dedup [x]      = [x]
dedup (x:y:ys) = x /= y `prependIf` x $ dedup (y:ys)

-- Remove the ith element of a list.
dropIth :: Int -> [a] -> [a]
dropIth 0 (_:xs) = xs
dropIth i (x:xs) = x : dropIth (i-1) xs

-- Safe version of (!!) which returns Nothing if the index was out of bounds. This was officially
-- added to Haskell in 2023 but Ubuntu's apt version of ghci doesn't have it yet.
(!?) :: [a] -> Int -> Maybe a
(!?) []     _ = Nothing
(!?) (x:_)  0 = Just x
(!?) (_:xs) i = xs !? (i-1)
infixl 9 !? -- the same as (!!)

-- For each element of the first list, find its index in the second.
elemsIndex :: Eq a => [a] -> [a] -> [Maybe Int]
elemsIndex xs ys = map (`elemIndex` ys) xs

-- Like elemIndex, but returns the index of the *last* matching element.
lastElemIndex :: Eq a => a -> [a] -> Maybe Int
lastElemIndex = lei' 0 where
    lei' _ _ []     = Nothing
    lei' i e (x:xs) | e == x    = case lei' (i+1) e xs of
                                      Nothing -> Just i
                                      Just i' -> Just i'
                    | otherwise = lei' (i+1) e xs

-- The number of occurrences of x in the list.
count :: Eq a => a -> [a] -> Int
count x = countBy (== x)

-- The number of elements of the list that satisfy the predicate.
countBy :: (a -> Bool) -> [a] -> Int
countBy f = length . filter f

-- Pair each element of a list with the next.
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs xs = zip xs (tail xs)

-- Remove the element at the given index from the list. Return (el, rest) where el is the removed
-- element and rest is the remaining list.
pick :: Int -> [a] -> (a, [a])
pick 0 (x:xs) = (x, xs)
pick i (x:xs) = let
    (choice, rest) = pick (i-1) xs
    in (choice, x:rest)

-- For each element x in the list yield (x, rest), where rest is the list sans x.
pickEach :: [a] -> [(a, [a])]
pickEach [x]    = [(x, [])]
pickEach (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (pickEach xs)

-- Whether the list is sorted in non-strictly increasing order according to the given comparison
-- function.
isSortedBy :: (a -> a -> Ordering) -> [a] -> Bool
isSortedBy _   []       = True
isSortedBy _   [x]      = True
isSortedBy cmp (x:y:ys) = (x `cmp` y) <= EQ && isSortedBy cmp (y:ys)

-- Whether the list is sorted in non-strictly increasing order.
isIncreasing :: Ord a => [a] -> Bool
isIncreasing = isSortedBy compare

-- Opposite of isIncreasing. This is equivalent to 'isIncreasing . reverse'.
isDecreasing :: Ord a => [a] -> Bool
isDecreasing = isSortedBy (flip  compare)

substrsOfLength :: Int -> [a] -> [[a]]
substrsOfLength n s@(_:xs) = if length s < n then []
                                             else take n s : substrsOfLength n xs

-- Like [x..y], but if y < x it produces a list that counts backwards from x to y
range' :: (Ord a, Enum a) => a -> a -> [a]
range' x y | x <= y    = [x..y]
           | otherwise = [x,(pred x)..y]


-- Matrices

type Index = (Int, Int)
type Bounds = (Index, Index)
type Matrix a = Array Index a


-- Whether the given bounds describe an empty matrix.
emptyBounds :: Bounds -> Bool
emptyBounds ((xLo,yLo),(xHi,yHi)) = xLo > xHi || yLo > yHi

-- Get the given component of the bounds.
--xLo' ((xLo,_),_) = xLo
--yLo' ((_,yLo),_) = yLo
--xHi' (_,(xHi,_)) = xHi
--yHi' (_,(_,yHi)) = yHi

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
    (lo@(xLo,yLo), hi@(xHi,yHi)) = bounds m
    lst = [[ m ! (x, y) | x <- [xLo..xHi]]
                        | y <- [yLo..yHi]]
    strs = map (map show) lst
    padLen = maximum $ map length $ concat strs
    leftPad str = replicate (padLen - length str) ' ' ++ str
    in ("[" ++) . (++ "] " ++ show lo ++ " -- " ++ show hi)
     $ intercalate "\n "
     $ map (intercalate " ")
     $ map (map leftPad) strs

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

