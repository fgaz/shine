module Point where

import qualified Prelude as P

type Point = (P.Double, P.Double)

(+) :: Point -> Point -> Point
(x0, y0) + (x1, y1) = (x0 P.+ x1, y0 P.+ y1)

(-) :: Point -> Point -> Point
(x0, y0) - (x1, y1) = (x0 P.- x1, y0 P.- y1)

(*) :: P.Double -> Point -> Point
k * (x, y) = (k P.* x, k P.* y)

negate :: Point -> Point
negate (x, y) = (P.negate x, P.negate y)

infixl 7 *
infixl 6 +
infixl 6 -

