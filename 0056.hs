-- https://leetcode.com/problems/merge-intervals/

type Interval = (Int, Int)

-- merge2 assumes that its first argument starts before or at the same point as its second argument.
-- It returns a Just Interval if its arguments overlap, or a Nothing if they're disjoint.
-- O(1) time, O(1) space
merge2 :: Interval -> Interval -> Maybe Interval
merge2 (xLo,xHi) (yLo,yHi) | yLo <= xHi = Just (xLo, max xHi yHi)
                           | otherwise  = Nothing

-- mergeSorted assumes that its input list is sorted by start points.
-- O(n) time, O(1) space
mergeSorted :: [Interval] -> [Interval]
mergeSorted [] = []
mergeSorted [x] = [x]
mergeSorted (x:y:rest) = case merge2 x y of
    Nothing -> x : mergeSorted (y:rest)
    Just xy -> mergeSorted (xy:rest)


-- O(n log n + n) = O(n log n) time, O(log n + 1) = O(log n) space
mergeIntervals :: [Interval] -> [Interval]
mergeIntervals = mergeSorted . sort

