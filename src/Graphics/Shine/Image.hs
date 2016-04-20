{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.Shine.Image (
  makeImage,
  ImageSize (..),
  ImageData (..),
) where

import GHCJS.DOM.HTMLImageElement

-- | Just a wrapper around the 'HTMLImageElement' type. Needed for the Show instance.
newtype ImageData = ImageData { unImageData :: HTMLImageElement } deriving Eq

-- we need this to show Pictures
instance Show ImageData where
    show _ = "ImageData"

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
makeImage :: FilePath -> IO ImageData
makeImage url = do
    img <- js_newImage
    setSrc img url
    return $ ImageData img
