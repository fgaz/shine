module Point where

import qualified Prelude as P

type Point a = (a, a)

(+) :: P.Num a => Point a -> Point a -> Point a
(x0, y0) + (x1, y1) = (x0 P.+ x1, y0 P.+ y1)

(-) :: P.Num a => Point a -> Point a -> Point a
(x0, y0) - (x1, y1) = (x0 P.- x1, y0 P.- y1)

(*) :: P.Num a => a -> Point a -> Point a
k * (x, y) = (k P.* x, k P.* y)

negate :: P.Num a => Point a -> Point a
negate (x, y) = (P.negate x, P.negate y)

infixl 7 *
infixl 6 +
infixl 6 -

