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


type Font = String

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
             -- | Draws some text (the float is the max width, the font is in js-style (es. "12px Sans"))
             | Text Font TextAlignment Float String
             -- | Draws an image
             | Image ImageSize HTMLImageElement
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
             deriving Eq --TODO show, with a newtype for Image

-- | A circle from the center coordinates and radius
circle :: Float -> Picture
circle r = Arc r 0 (2*3.14) False

path :: [(Float,Float)] -> Picture
path xs = foldMap (\((x,y),(x',y')) -> Line x y x' y') $ zip xs $ tail xs

instance Monoid Picture where
    mempty = Empty
    mappend = Over

instance Show Picture where
