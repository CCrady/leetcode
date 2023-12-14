-- https://leetcode.com/problems/swap-nodes-in-pairs/

-- pattern matching, my beloved
swapElems []         = []
swapElems [x]        = [x]
swapElems (x:y:rest) = y : x : swapElems rest

