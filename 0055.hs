-- https://leetcode.com/problems/jump-game/

import Data.List


-- TODO: test and clean up
--       canJump' could be implemented as a foldr instead of explicit recursion?
canJump :: [Int] -> Bool
canJump = canJump' 0 where
    canJump' (-1)  _      = False
    canJump' _     [_]    = True
    canJump' reach (x:xs) = canJump' (max reach x - 1) xs

