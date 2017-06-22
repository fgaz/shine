{-|
Module      : Graphics.Shine.Picture
Description : Short description
Copyright   : (c) Francesco Gazzetta, 2016
License     : MIT
Maintainer  : francygazz@gmail.com
Stability   : experimental

This module contains the 'Picture' datatype, used to represent the image to draw
on the canvas, and some functions to operate on it.
-}
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

-- | Js-style font, ex. @"12px Sans"@
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
             | Rect Double Double
             -- | Same thing but filled
             | RectF Float Float
             -- | A line from the coordinates of two points
             | Line Double Double Double Double
             -- | A polygon from a list of vertices
             | Polygon [(Double, Double)]
             -- | An arc from the radius, start angle, end angle.
             -- If the last parameter is True, the direction is counterclockwise
             -- TODO replace with Clockwise | Counterclockwise or remove entirely
             | Arc Double Double Double Bool
             -- | A filled circle from the radius
             | CircleF Double
             -- | Draws some text. The 'Maybe' 'Float' is the max width.
             | Text Font TextAlignment (Maybe Float) String
             -- | Draws an image
             | Image ImageSize ImageData
             -- | Draws the second `Picture` over the first
             | Over Picture Picture
             -- | Applies the `Color` to the picture.
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
circle :: Double -> Picture
circle r = Arc r 0 (2*3.14) False

-- | Shorthand to draw a series of lines
path :: [(Double, Double)] -> Picture
path xs = foldMap (\((x,y),(x',y')) -> Line x y x' y') $ zip xs $ tail xs

-- | 'Picture's are 'Monoid's. The identity is an 'Empty' (completely transparent)
-- picture and the composing function is the overlapping (the right picture is
-- drawn over the left one).
instance Monoid Picture where
    mempty = Empty
    mappend = Over
