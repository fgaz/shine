module Graphics.Shine (
  Picture (..),
  Color (..),
  TextAlignment (..),
  circle,
  path,
  animate,
  animateIO,
  play,
  playIO,
  (<>)
) where

import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody, getElementById, mouseUp, mouseDown, mouseMove, wheel, keyDown, keyUp)
import GHCJS.DOM.EventM (on, mouseButton, mouseCtrlKey, mouseAltKey, mouseShiftKey, mouseMetaKey, mouseClientXY, uiKeyCode, event)
import GHCJS.DOM.WheelEvent (getDeltaX, getDeltaY)
import GHCJS.DOM.KeyboardEvent (KeyboardEvent, getCtrlKey, getShiftKey, getAltKey, getMetaKey)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.HTMLImageElement (getWidth, getHeight)
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.Enums (CanvasWindingRule (CanvasWindingRuleNonzero))
import GHCJS.DOM.Types (Window, CanvasStyle (..), Document, MouseEvent)

import GHCJS.Prim (JSVal, toJSString)
import Web.KeyCode (keyCodeLookup)
import Unsafe.Coerce (unsafeCoerce)
import Data.Monoid ((<>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, modifyMVar, modifyMVar_)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (foldrM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)

import Graphics.Shine.Input
import Graphics.Shine.Image

type Font = String

data TextAlignment = LeftAlign | CenterAlign | RightAlign

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
             -- TODO stroke, polygon

-- | A circle from the center coordinates and radius
circle :: Float -> Picture
circle r = Arc r 0 (2*3.14) False

path :: [(Float,Float)] -> Picture
path xs = foldMap (\((x,y),(x',y')) -> Line x y x' y') $ zip xs $ tail xs

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
     -> (Input -> state -> state) --MAYBE flip args
     -> (Float -> state -> state) --MAYBE flip args
     -> IO ()
play fps xy initialState draw' {-rename draw to render-} handleInput step =
  playIO
    fps
    xy
    initialState
    (\_ s -> return $ draw' s)
    (\_ s i -> return $ handleInput s i)
    (\_ s t -> return $ step s t)

getModifiersMouse :: ReaderT MouseEvent IO Modifiers
getModifiersMouse = Modifiers
                   <$> fmap toKeyState mouseCtrlKey
                   <*> fmap toKeyState mouseAltKey
                   <*> fmap toKeyState mouseShiftKey
                   <*> fmap toKeyState mouseMetaKey

getModifiersKeyboard :: ReaderT KeyboardEvent IO Modifiers
getModifiersKeyboard = Modifiers
                   <$> fmap toKeyState (event >>= getCtrlKey)
                   <*> fmap toKeyState (event >>= getAltKey)
                   <*> fmap toKeyState (event >>= getShiftKey)
                   <*> fmap toKeyState (event >>= getMetaKey)

playIO :: Float -- ^ FPS
       -> (Int,Int) -- ^ Canvas dimensions
       -> state
       -> (CanvasRenderingContext2D -> state -> IO Picture) -- ^ Your drawing function
       -> (CanvasRenderingContext2D -> Input -> state -> IO state)
       -> (CanvasRenderingContext2D -> Float -> state -> IO state)
       -> IO ()
playIO fps (x,y) initialState draw' {-rename draw to render-} handleInput step = runWebGUI $ \ webView -> do
    (doc, ctx) <- initCanvas webView x y
    inputM <- newMVar []
    _ <- on doc mouseDown $ do
        btn <- fmap toMouseButton mouseButton
        modifiers <- getModifiersMouse
        when (isJust btn) $
          liftIO $ modifyMVar_ inputM $ fmap return (MouseButton (fromJust btn) Down modifiers :) -- :-) :D XD
    _ <- on doc mouseUp $ do
        btn <- fmap toMouseButton mouseButton
        modifiers <- getModifiersMouse
        when (isJust btn) $
          liftIO $ modifyMVar_ inputM $ fmap return (MouseButton (fromJust btn) Up modifiers :) -- :-) :D XD
    _ <- on doc mouseMove $ do
        coords <- mouseClientXY
        liftIO $ modifyMVar_ inputM $ fmap return (MouseMove coords :) -- :-) :D XD
    _ <- on doc wheel $ do
        delta <- (,) <$> (event >>= getDeltaX) <*> (event >>= getDeltaY)
        liftIO $ modifyMVar_ inputM $ fmap return (MouseWheel delta :) -- :-) :D XD
    _ <- on doc keyDown $ do
        key <- uiKeyCode
        modifiers <- getModifiersKeyboard
        liftIO $ modifyMVar_ inputM $ fmap return (Keyboard (keyCodeLookup key) Down modifiers :) -- :-) :D XD
    _ <- on doc keyUp $ do
        key <- uiKeyCode
        modifiers <- getModifiersKeyboard
        liftIO $ modifyMVar_ inputM $ fmap return (Keyboard (keyCodeLookup key) Up modifiers :) -- :-) :D XD
    let loop state = do
        stamp <- getCurrentTime
        inputs <- modifyMVar inputM $ \xs -> return ([], xs)
        state' <- foldrM (handleInput ctx) state inputs
        state'' <- step ctx (1/fps) state' --MAYBE change 1/fps to actual td
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
draw ctx (Polygon ((x,y):pts)) = do
    beginPath ctx
    moveTo ctx x y
    mapM_ (\(x', y') -> lineTo ctx x' y') pts
    closePath ctx
    fill ctx CanvasWindingRuleNonzero
draw ctx (Polygon []) = draw ctx Empty
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
draw ctx (Text font align width txt) = do
    setFont ctx font
    setTextAlign ctx $ case align of LeftAlign -> "left"
                                     CenterAlign -> "center"
                                     RightAlign -> "rignt"
    fillText ctx txt 0 0 width
draw ctx (Image size img) = do
    case size of
      Original -> do
          x <- fmap ((/(-2)) . realToFrac) $ getWidth img
          y <- fmap ((/(-2)) . realToFrac) $ getHeight img
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
draw ctx (Over a b) = do
    draw ctx a
    draw ctx b
draw ctx (Colored col (Over a b)) = draw ctx $ Over (Colored col a)
                                                    (Colored col b)
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
