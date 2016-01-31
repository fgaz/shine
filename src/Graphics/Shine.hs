module Graphics.Shine (
  Picture (..),
  Color (..),
  circle,
  animate,
  animateIO,
  play,
  playIO,
  (<>)
) where

import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody, getElementById, mouseUp, mouseDown)
import GHCJS.DOM.EventM (on, mouseButton, mouseCtrlKey, mouseAltKey, mouseShiftKey, mouseMetaKey)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.Enums (CanvasWindingRule (CanvasWindingRuleNonzero))
import GHCJS.DOM.Types (Window, CanvasStyle (..), Document, MouseEvent)

import GHCJS.Prim (JSVal, toJSString)
import Unsafe.Coerce (unsafeCoerce)
import Data.Monoid ((<>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, modifyMVar, modifyMVar_)
import Control.Monad (when, foldM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)

import Graphics.Shine.Input

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

initCanvas :: Window -> Int -> Int -> IO (Document, CanvasRenderingContext2D)
initCanvas webView x y = do
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    setInnerHTML body (Just $ canvasHtml x y)
    Just c <- getElementById doc "canvas"
    ctx <- getContext (unsafeCoerce c) "2d" :: IO JSVal
    return (doc, unsafeCoerce ctx) --how do i get a 2dcontext properly? This works for now.

-- | Draws a picture which depends only on the time
animate :: Float -- ^ FPS
        -> (Int,Int) -- ^ Canvas dimensions
        -> (Float -> Picture) -- ^ Your drawing function
        -> IO ()
animate fps xy f = animateIO fps xy $ const (return . f)

-- | Draws a picture which depends only on the time... and everything else,
-- since you can do I/O.
animateIO :: Float -- ^ FPS
          -> (Int,Int) -- ^ Canvas dimensions
          -> (CanvasRenderingContext2D -> Float -> IO Picture) -- ^ Your drawing function
          -> IO ()
animateIO fps (x,y) f = runWebGUI $ \ webView -> do
    (_, ctx) <- initCanvas webView x y
    let loop t = do
        stamp <- getCurrentTime
        clearRect ctx 0 0 (fromIntegral x) (fromIntegral y)
        setTransform ctx 1 0 0 1 0 0 -- reset transforms (and accumulated errors!).
        pic <- f ctx t
        draw ctx pic
        now <- getCurrentTime
        let td = diffUTCTime now stamp
        when (realToFrac td <= 1 / fps) $
          threadDelay $ floor $ (*1000000) (1 / fps - realToFrac td)
        loop (t + 1/fps) --MAYBE change to currentTime - startTime
      in
        loop 0

play :: Float
     -> (Int, Int)
     -> state
     -> (state -> Picture)
     -> (state -> Input -> state) --MAYBE flip args
     -> (state -> Float -> state) --MAYBE flip args
     -> IO ()
play fps xy initialState draw' {-rename draw to render-} handleInput step =
  playIO
    fps
    xy
    initialState
    (\_ s -> return $ draw' s)
    (\_ s i -> return $ handleInput s i)
    (\_ s t -> return $ step s t)

getModifiers :: ReaderT MouseEvent IO Modifiers
getModifiers = Modifiers
                   <$> fmap toKeyState mouseCtrlKey
                   <*> fmap toKeyState mouseAltKey
                   <*> fmap toKeyState mouseShiftKey
                   <*> fmap toKeyState mouseMetaKey

playIO :: Float -- ^ FPS
       -> (Int,Int) -- ^ Canvas dimensions
       -> state
       -> (CanvasRenderingContext2D -> state -> IO Picture) -- ^ Your drawing function
       -> (CanvasRenderingContext2D -> state -> Input -> IO state)
       -> (CanvasRenderingContext2D -> state -> Float -> IO state)
       -> IO ()
playIO fps (x,y) initialState draw' {-rename draw to render-} handleInput step = runWebGUI $ \ webView -> do
    (doc, ctx) <- initCanvas webView x y
    inputM <- newMVar []
    _ <- on doc mouseDown $ do
        btn <- fmap toMouseButton mouseButton
        modifiers <- getModifiers
        when (isJust btn) $
          liftIO $ modifyMVar_ inputM $ fmap return (MouseButton (fromJust btn) Down modifiers :) -- :-) :D XD
    _ <- on doc mouseUp $ do
        btn <- fmap toMouseButton mouseButton
        modifiers <- getModifiers
        when (isJust btn) $
          liftIO $ modifyMVar_ inputM $ fmap return (MouseButton (fromJust btn) Up modifiers :) -- :-) :D XD
    let loop state = do
        stamp <- getCurrentTime
        inputs <- modifyMVar inputM $ \xs -> return ([], reverse xs)
        state' <- foldM (handleInput ctx) state inputs
        state'' <- step ctx state' (1/fps) --MAYBE change 1/fps to actual td
        clearRect ctx 0 0 (fromIntegral x) (fromIntegral y)
        setTransform ctx 1 0 0 1 0 0 -- reset transforms (and accumulated errors!).
        pic <- draw' ctx state''
        draw ctx pic
        now <- getCurrentTime
        let td = diffUTCTime now stamp
        when (realToFrac td <= 1 / fps) $
          threadDelay $ floor $ (*1000000) (1 / fps - realToFrac td)
        loop state''
      in
        loop initialState

canvasHtml :: Int -> Int -> String
canvasHtml x y = "<canvas id=\"canvas\" \
             \width=\""++ show x ++ "\" \
             \height=\""++ show y ++ "\" \
             \style=\"border:1px \
             \solid #000000;\">\
             \</canvas> "

draw :: CanvasRenderingContext2D -> Picture -> IO ()
draw _ Empty = return ()
draw ctx (Line x y x' y') = do
    moveTo ctx x y
    lineTo ctx x' y'
    stroke ctx
draw ctx (Rect x y) = do
    rect ctx (-x/2) (-y/2) x y
    stroke ctx
draw ctx (RectF x y) = fillRect ctx (-x/2) (-y/2) x y
draw ctx (Arc r a b direction) = do
    beginPath ctx
    arc ctx 0 0 r a b direction
    stroke ctx
draw ctx (CircleF r) = do
    save ctx
    draw ctx $ circle r
    clip ctx CanvasWindingRuleNonzero
    draw ctx $ RectF (r*2) (r*2)
    restore ctx
draw ctx (Over a b) = do
    draw ctx a
    draw ctx b
draw ctx (Colored col (Over a b)) = do
    draw ctx $ Colored col a
    draw ctx $ Colored col b
draw ctx (Colored _ (Colored col pic)) =
    draw ctx $ Colored col pic --the innermost color wins
draw ctx (Colored (Color r g b a) pic) = do
    let colorString = "rgba("
                   ++ intercalate "," [show r, show g, show b, show a]
                   ++ ")"
    let color = toJSString colorString
    setFillStyle ctx $ Just $ CanvasStyle color
    setStrokeStyle ctx $ Just $ CanvasStyle color
    draw ctx pic
    -- set the color back to black
    let black = toJSString "#000000"
    setFillStyle ctx $ Just $ CanvasStyle black
    setStrokeStyle ctx $ Just $ CanvasStyle black
draw ctx (Rotate angle pic) = do
    rotate ctx angle
    draw ctx pic
    --setTransform ctx 1 0 0 1 0 0 --not ok: prevents Rotate composition
    rotate ctx (-angle)
draw ctx (Translate x y pic) = do
    translate ctx x y
    draw ctx pic
    translate ctx (-x) (-y)
