-- https://leetcode.com/problems/roman-to-integer/

val 'I' =    1
val 'V' =    5
val 'X' =   10
val 'L' =   50
val 'C' =  100
val 'D' =  500
val 'M' = 1000

rom2int :: String -> Int
rom2int s = let
    -- given a list of n copies of a character, find the value that they collectively represent
    run_val cs = val (head cs) * length cs
    -- turn the input string into the values of each run of characters
    run_vals = map (run_val) (group s)
    -- For additive roman numerals, run A's value must be at least as large as the value of the
    -- string to its right (otherwise you'd just use another copy of character A). If this rule is
    -- broken, i.e. run A's value is less than the accumulated value to its right, then A must be
    -- using the subtraction rule.
    f e acc = if e < acc then acc - e else acc + e
    in foldr f 0 run_vals

