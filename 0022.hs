-- https://leetcode.com/problems/generate-parentheses/

allParens :: Int -> [String]
allParens n = allParens' 0 (2*n) where
    allParens' nOpen n2go
        -- if we have more dangling open parens than we have space to close, then there's no
        -- possible valid string
        | nOpen > n2go            = []
        -- the valid base case
        | nOpen == 0 && n2go == 0 = [""]
        -- if we don't have any dangling open parens, then the only possibility is to open a new one
        | nOpen == 0              = map ('(':) $ allParens' (nOpen+1) (n2go-1)
        -- if we have at least one dangling open paren, then closing it is a valid possiblity as
        -- well as opening a new one
        -- 'l ++ r' is linear in the length of l, so it's best to put the shorter operand first when
        -- possible. I think adding the open paren should generally produce a shorter list than the
        -- close paren, since the higher value of nOpen puts a greater restriction on what can come
        -- next, but I could be wrong. In any case, this is the order that exactly matches example 1
        -- from the problem statement, so I'm keeping it this way.
        | otherwise               = (map ('(':) $ allParens' (nOpen+1) (n2go-1))
                                 ++ (map (')':) $ allParens' (nOpen-1) (n2go-1))

