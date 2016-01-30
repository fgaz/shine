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
import GHCJS.DOM.Types (Window, CanvasStyle (..))

import GHCJS.Prim (JSVal)
import GHCJS.Marshal
import Unsafe.Coerce (unsafeCoerce)
import Data.Monoid ((<>))
import Control.Concurrent (threadDelay)
import Data.List (intercalate)

-- | A color given r, g, b (all from 0 to 255) and alpha (from 0 to 1)
data Color = Color Int Int Int Float

-- | A drawable element. All Pictures are centered.
data Picture = Empty -- ^ The empty picture. Draws nothing.
             -- | A rectangle from the dimensions
             | Rect Float Float
             -- | Same thing but filled
             | RectF Float Float
             -- | A line from the coordinates of two points
             | Line Float Float Float Float
             -- | An arc from the radius, start angle, end angle.
             -- If the last parameter is True, the direction is counterclockwise
             -- TODO replace with Clockwise | Counterclockwise or remove entirely
             | Arc Float Float Float Bool
             -- | A filled circle from the radius
             | CircleF Float
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

-- | A circle from the center coordinates and radius
circle :: Float -> Picture
circle r = Arc r 0 (2*3.14) False

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

-- | Draws a picture which depends only on the time
animate :: (Int,Int) -> (Float -> Picture) -> IO ()
animate xy f = animateIO xy $ const (return . f)

-- | Draws a picture which depends only on the time... and everything else,
-- since you can do I/O.
animateIO :: (Int,Int) -> (CanvasRenderingContext2D -> Float -> IO Picture) -> IO ()
animateIO (x,y) f = runWebGUI $ \ webView -> do
    ctx <- initCanvas webView x y
    let loop t = do
        clearRect ctx 0 0 (fromIntegral x) (fromIntegral y)
        setTransform ctx 1 0 0 1 0 0 -- reset transforms (and accumulated errors!).
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
draw ctx (Rect c d) = do
    rect ctx (-c/2) (-d/2) c d
    stroke ctx
draw ctx (RectF c d) = fillRect ctx (-c/2) (-d/2) c d
draw ctx (Arc c d e f) = do
    beginPath ctx
    arc ctx 0 0 c d e f
    stroke ctx
draw ctx (CircleF r) = do
    save ctx
    draw ctx (circle r)
    clip ctx CanvasWindingRuleNonzero
    draw ctx (RectF (r*2) (r*2))
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
    let colorString = "rgba("
                   ++ intercalate "," [show r, show g, show b, show a]
                   ++ ")"
    color <- toJSVal colorString
    setFillStyle ctx $ Just $ CanvasStyle color
    setStrokeStyle ctx $ Just $ CanvasStyle color
    draw ctx x
    -- set the color back to black
    black <- toJSVal "#000000"
    setFillStyle ctx $ Just $ CanvasStyle black
    setStrokeStyle ctx $ Just $ CanvasStyle black
draw ctx (Rotate angle pic) = do
    rotate ctx angle
    draw ctx pic
    --setTransform ctx 1 0 0 1 0 0 --not ok: prevents Rotate composition
    rotate ctx (-angle)
draw ctx (Translate a b pic) = do
    translate ctx a b
    draw ctx pic
    translate ctx (-a) (-b)
