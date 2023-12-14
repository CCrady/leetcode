-- https://leetcode.com/problems/3sum-closest/

import Leet (triples)
import Data.List
import Data.Function (on)


threeSumClosest :: Int -> [Int] -> Int
-- 'compare `on` abs `on` (t-)' is a fancy way of saying that we want to compare elements by taking
-- their difference from t. If an element is further from t, then it will be compared as GT an
-- element closer to t. Because `on` is left-associative, this is equivalent to the expression
-- 'compare `on` (abs . (t-))'; I'm not sure which is more readable.
threeSumClosest t = minimumBy (compare `on` abs `on` (t-))
                  . map (\(x, y, z) -> x + y + z)
                  . triples

-------- A (KIND OF JANKY) PROOF THAT 'a `on` b `on` c' IS EQUIVALENT TO 'a `on` (b . c)'
---- 'on' is just the following combinator:
---- P = \x\y\z\w.x(yz)(yw)
---- Composition ('.' in haskell) is just the following combinator:
---- B = \x\y\z.x(yz)
-- a `on` b `on` c
-- (a `on` b) `on` c
-- on (on a b) c
--  Translated to lambda calculus:
-- P(Pab)c
-- (\x\y\z\w.x(yz)(yw))(Pab)c
-- (\y\z\w.(Pab)(yz)(yw))c
-- \z\w.(Pab)(cz)(cw)
-- \z\w.((\p\q\r\s.p(qr)(qs))ab)(cz)(cw)
-- \z\w.((\q\r\s.a(qr)(qs))b)(cz)(cw)
-- \z\w.(\r\s.a(br)(bs))(cz)(cw)
-- \z\w.(\s.a(b(cz))(bs))(cw)
-- \z\w.a(b(cz))(b(cw))
-- \z\w.a((\l\m\n.l(mn))bcz)((\l\m\n.l(mn))bcw)
-- \z\w.a(Bbcz)(Bbcw)
-- \z\w.a((Bbc)z)((Bbc)w)
-- (\y\z\w.a(yz)(yw))(Bbc)
-- (\x\y\z\w.x(yz)(yw))a(Bbc)
-- (P)a(Bbc)
-- Pa(Bbc)
--  Translated back to haskell:
-- on a ((.) b c)
-- a `on` (b . c)

