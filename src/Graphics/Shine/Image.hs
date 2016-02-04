module Graphics.Shine.Image (
  makeImage,
  ImageSize (..),
  HTMLImageElement
) where

import GHCJS.DOM.HTMLImageElement

data ImageSize = Original -- ^ The orizinal size of the file
               -- | Scale the image to the given dimensions
               | Stretched Float Float
               -- | Clip the image from the given coordinates to the given width and height
               | Clipped Float Float Float Float
               -- | Clip (x,y,width,height) and scale (width, height) the image
               | ClippedStretched Float Float Float Float Float Float

foreign import javascript unsafe "$r = new Image();"
    js_newImage        :: IO HTMLImageElement


makeImage :: FilePath -> IO HTMLImageElement
makeImage url = do
    img <- js_newImage
    setSrc img url
    return img
