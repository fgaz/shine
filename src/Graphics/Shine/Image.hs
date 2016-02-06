{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Shine.Image (
  makeImage,
  ImageSize (..),
  HTMLImageElement
) where

import GHCJS.DOM.HTMLImageElement

-- we need this to show Pictures
instance Show HTMLImageElement where
    show _ = "HTMLImageElement"

-- | How big (and how stretched/cropped) the Image is drawn
data ImageSize =
    -- | The orizinal size of the image
    Original
    -- | Scale the image to the given dimensions
    | Stretched Float Float
    -- | Clip the image from the given coordinates to the given width and height
    | Clipped Float Float Float Float
    -- | Clip (x,y,width,height) and scale (width, height) the image
    | ClippedStretched Float Float Float Float Float Float
    deriving (Eq, Show)

foreign import javascript unsafe "$r = new Image();"
    js_newImage        :: IO HTMLImageElement


-- | Makes an image element from an URL
makeImage :: FilePath -> IO HTMLImageElement
makeImage url = do
    img <- js_newImage
    setSrc img url
    return img
