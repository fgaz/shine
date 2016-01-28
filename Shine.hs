module Shine (
  Picture (..),
  Color (..),
  circle,
  animate,
  (<>)
) where

import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.Enums (CanvasWindingRule (CanvasWindingRuleNonzero))
import GHCJS.DOM.Document (getElementById)
import GHCJS.DOM.Types (Window, IsCanvasStyle, CanvasStyle, toCanvasStyle)

import GHCJS.Prim (JSVal)
import Unsafe.Coerce (unsafeCoerce)
import Data.Monoid ((<>))
import Control.Concurrent (threadDelay)

data Color = Color Float Float Float Float

data Picture = Empty
             | Rect Float Float Float Float
             | RectF Float Float Float Float
             | Line Float Float Float Float
             | Arc Float Float Float Float Float Bool
             | CircleF Float Float Float
             | Over Picture Picture
             | Colored Color Picture --TODO

circle :: Float -> Float -> Float -> Picture
circle a b r = Arc a b r 0 (2*3.14) False

instance Monoid Picture where
    mempty = Empty
    mappend = Over

initCanvas :: Window -> Int -> Int -> IO CanvasRenderingContext2D
initCanvas webView x y = do
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    setInnerHTML body (Just $ canvasHtml x y)
    Just c <- getElementById doc "canvas"
    ctx <- (getContext (unsafeCoerce c) "2d") :: IO (JSVal)
    return $ unsafeCoerce ctx --how do i get a 2dcontext properly? This works for now.

animate :: (Int,Int) -> (Float -> Picture) -> IO ()
animate xy f = animateIO xy $ const (return . f)

animateIO :: (Int,Int) -> (CanvasRenderingContext2D -> Float -> IO Picture) -> IO ()
animateIO (x,y) f = runWebGUI $ \ webView -> do
    ctx <- initCanvas webView x y
    let loop t = do
        clearRect ctx 0 0 (fromIntegral x) (fromIntegral y)
        pic <- f ctx t
        draw ctx pic
        threadDelay 30000 --TODO FPS capping
        loop (t+100) --TODO add the time delta
      in
        loop 0

canvasHtml :: Int -> Int -> String
canvasHtml x y = "<canvas id=\"canvas\" \
             \width=\""++ show x ++ "\" \
             \height=\""++ show y ++ "\" \
             \style=\"border:1px \
             \solid #000000;\">\
             \</canvas> "

draw :: CanvasRenderingContext2D -> Picture -> IO ()
draw _ Empty = return ()
draw ctx (Line a b c d) = do
    moveTo ctx a b
    lineTo ctx c d
    stroke ctx
draw ctx (Rect a b c d) = do
    rect ctx a b c d
    stroke ctx
draw ctx (RectF a b c d) = fillRect ctx a b c d
draw ctx (Arc a b c d e f) = do
    beginPath ctx
    arc ctx a b c d e f
    stroke ctx
draw ctx (CircleF x y r) = do
    save ctx
    draw ctx (circle x y r)
    clip ctx CanvasWindingRuleNonzero
    draw ctx (RectF (x-r) (y-r) (r*2) (r*2))
    restore ctx
draw ctx (Over a b) = do
    draw ctx a
    draw ctx b
draw ctx (Colored col (Over a b)) = do
    draw ctx $ Colored col a
    draw ctx $ Colored col b
draw ctx (Colored _ (Colored col a)) = do --the innermost color wins
    draw ctx $ Colored col a
draw ctx (Colored (Color r g b a) x) = do
    -- TODO none of this works ↓
    --setStrokeColorRGB ctx r g b a
    --setFillColorRGB ctx r g b a
    --setFillStyle ctx (Just $  (unsafeCoerce "#ff0000" :: CanvasStyle))
    draw ctx x
    --setcolor black? --TODO

--TODO this probably needs to be a Picture since the rotation is handled by js.
rotate :: Float -> Picture -> Picture
rotate angle _ = undefined

--TODO this can be a function
move :: Float -> Float -> Picture -> Picture
move x y _ = undefined
