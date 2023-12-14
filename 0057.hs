-- https://leetcode.com/problems/insert-interval/

type Interval = (Int, Int)

areOverlapping :: Interval -> Interval -> Bool
areOverlapping (xLo,xHi) (yLo,yHi) = not $ xLo > yHi || yLo > xHi

merge2 :: Interval -> Interval -> Maybe Interval
merge2 x@(xLo,xHi) y@(yLo,yHi) | areOverlapping x y = Just (min xLo yLo, max xHi yHi)
                               | otherwise  = Nothing

insert :: Interval -> [Interval] -> [Interval]
insert x [] = [x]
insert x@(_,xHi) (y@(yLo,_):ys) | xHi < yLo = x:y:ys
insert x (y:ys) = case merge2 x y of
    Just xy -> insert xy ys
    Nothing -> y : insert x ys

