module Graphics.Shine.Picture (
  Picture (..),
  Color (..),
  TextAlignment (..),
  Font,
  circle,
  path,
  (<>)
) where

import Data.Monoid ((<>))
import Graphics.Shine.Image

-- | Js-style font, ex. "12px Sans"
type Font = String

-- | How the text should be aligned
data TextAlignment = LeftAlign | CenterAlign | RightAlign deriving (Eq, Show)

-- | A color given r, g, b (all from 0 to 255) and alpha (from 0 to 1)
data Color = Color Int Int Int Float deriving (Eq, Show)

-- | A drawable element. All Pictures are centered.
data Picture =
             -- | The empty picture. Draws nothing.
             Empty
             -- | A rectangle from the dimensions
             | Rect Float Float
             -- | Same thing but filled
             | RectF Float Float
             -- | A line from the coordinates of two points
             | Line Float Float Float Float
             -- | A polygon from a list of vertices
             | Polygon [(Float, Float)]
             -- | An arc from the radius, start angle, end angle.
             -- If the last parameter is True, the direction is counterclockwise
             -- TODO replace with Clockwise | Counterclockwise or remove entirely
             | Arc Float Float Float Bool
             -- | A filled circle from the radius
             | CircleF Float
             -- | Draws some text (the float is the max width, the font is in js-style (ex. "12px Sans"))
             | Text Font TextAlignment Float String
             -- | Draws an image
             | Image ImageSize ImageData
             -- | Draws the second Picture over the First
             | Over Picture Picture
             -- | Applies the color to the picture.
             -- Innermost colors have the precedence, so you can set a "global
             -- color" and override it
             | Colored Color Picture
             -- | Rotates the Picture (in radians)
             | Rotate Float Picture
             -- | Moves the Picture by the given x and y distances
             | Translate Float Float Picture
             -- TODO stroke
             deriving (Eq, Show)

-- | A circle from the center coordinates and radius
circle :: Float -> Picture
circle r = Arc r 0 (2*3.14) False

-- | Shorthand to draw a series of lines
path :: [(Float,Float)] -> Picture
path xs = foldMap (\((x,y),(x',y')) -> Line x y x' y') $ zip xs $ tail xs

-- | Pictures are Monoids. The identity is an empty (completely transparent)
-- picture and the composing function is the overlapping (the right picture is
-- drawn over the left one).
instance Monoid Picture where
    mempty = Empty
    mappend = Over
