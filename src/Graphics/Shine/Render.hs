module Graphics.Shine.Render (
  render
) where

import GHCJS.DOM.HTMLImageElement (getWidth, getHeight)
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.Enums (CanvasWindingRule (CanvasWindingRuleNonzero))
import GHCJS.DOM.Types (CanvasStyle (..))

import GHCJS.Prim (toJSString)
import Data.List (intercalate)

import Graphics.Shine.Picture
import Graphics.Shine.Image


-- | Renders a picture on a 2D context.
render :: CanvasRenderingContext2D -> Picture -> IO ()
render _ Empty = return ()
render ctx (Line x y x' y') = do
    moveTo ctx x y
    lineTo ctx x' y'
    stroke ctx
render ctx (Rect x y) = do
    rect ctx (-x/2) (-y/2) x y
    stroke ctx
render ctx (RectF x y) = fillRect ctx (-x/2) (-y/2) x y
render ctx (Polygon ((x,y):pts)) = do
    beginPath ctx
    moveTo ctx x y
    mapM_ (uncurry (lineTo ctx)) pts
    closePath ctx
    fill ctx CanvasWindingRuleNonzero
render ctx (Polygon []) = render ctx Empty
render ctx (Arc r a b direction) = do
    beginPath ctx
    arc ctx 0 0 r a b direction
    stroke ctx
render ctx (CircleF r) = do
    save ctx
    render ctx $ circle r
    clip ctx CanvasWindingRuleNonzero
    render ctx $ RectF (r*2) (r*2)
    restore ctx
render ctx (Text font align width txt) = do
    setFont ctx font
    setTextAlign ctx $ case align of LeftAlign -> "left"
                                     CenterAlign -> "center"
                                     RightAlign -> "rignt"
    fillText ctx txt 0 0 width
render ctx (Image size img) =
    case size of
      Original -> do
          x <- ((/(-2)) . realToFrac) <$> getWidth img
          y <- ((/(-2)) . realToFrac) <$> getHeight img
          drawImage ctx (Just img) x y
      (Stretched w h) -> do
          let (x, y) = (-w/2, -h/2)
          drawImageScaled ctx (Just img) x y w h
      (Clipped a b c d) -> do
          let (x, y) = (-c/2, -d/2)
          drawImagePart ctx (Just img) a b c d x y c d
      (ClippedStretched a b c d e f) -> do
          let (x, y) = (-e/2, -f/2)
          drawImagePart ctx (Just img) a b c d x y e f
render ctx (Over a b) = do
    render ctx a
    render ctx b
render ctx (Colored col (Over a b)) = render ctx $ Over (Colored col a)
                                                    (Colored col b)
-- push all the Colors to the leaves to avoid things like
-- Color blue $ Translate _ _ $ Over (Color red pic) pic'q
-- in which pic' would be black instead of blue:
-- do
--   set color blue -- first Colored
--   translate
--   set color red -- second Colored
--   render pic
--   set color back to black -- second Colored
--   render pic' -- now this is black!
--   translate back
--   set color back to black -- first Colored
render ctx (Colored col (Rotate angle pic)) =
    render ctx $ Rotate angle $ Colored col pic
render ctx (Colored col (Translate x y pic)) =
    render ctx $ Translate x y $ Colored col pic
render ctx (Colored _ (Colored col pic)) =
    render ctx $ Colored col pic --the innermost color wins
render ctx (Colored (Color r g b a) pic) = do
    let colorString = "rgba("
                   ++ intercalate "," [show r, show g, show b, show a]
                   ++ ")"
    let color = toJSString colorString
    setFillStyle ctx $ Just $ CanvasStyle color
    setStrokeStyle ctx $ Just $ CanvasStyle color
    render ctx pic
    -- set the color back to black
    let black = toJSString "#000000"
    setFillStyle ctx $ Just $ CanvasStyle black
    setStrokeStyle ctx $ Just $ CanvasStyle black
render ctx (Rotate angle pic) = do
    rotate ctx angle
    render ctx pic
    --setTransform ctx 1 0 0 1 0 0 --not ok: prevents Rotate composition
    rotate ctx (-angle)
render ctx (Translate x y pic) = do
    translate ctx x y
    render ctx pic
    translate ctx (-x) (-y)
