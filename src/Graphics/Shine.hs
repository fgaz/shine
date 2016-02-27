module Graphics.Shine (
  toContext,
  fullScreenCanvas,
  fixedSizeCanvas,
  animate,
  animateIO,
  play,
  playIO
) where

import GHCJS.DOM (webViewGetDomDocument)
import GHCJS.DOM.Document (getBody, getElementById, mouseUp, mouseDown, mouseMove, wheel, keyDown, keyUp)
import GHCJS.DOM.EventM (on, mouseButton, mouseCtrlKey, mouseAltKey, mouseShiftKey, mouseMetaKey, mouseOffsetXY, uiKeyCode, event)
import GHCJS.DOM.EventTarget (IsEventTarget)
import GHCJS.DOM.WheelEvent (getDeltaX, getDeltaY)
import GHCJS.DOM.KeyboardEvent (KeyboardEvent, getCtrlKey, getShiftKey, getAltKey, getMetaKey)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.Types (Window, Element, MouseEvent, IsDocument)

import GHCJS.Prim (JSVal)
import Web.KeyCode (keyCodeLookup)
import Unsafe.Coerce (unsafeCoerce)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar, modifyMVar, modifyMVar_)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (foldrM)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (isJust, fromJust)

import Graphics.Shine.Input
import Graphics.Shine.Picture
import Graphics.Shine.Render

-- | Get a context from a canvas element.
toContext :: Element -- ^ this **must** be a canvas
          -> IO CanvasRenderingContext2D
toContext c = do
    ctx <- getContext (unsafeCoerce c) "2d" :: IO JSVal
    return $ unsafeCoerce ctx --how do i get a 2dcontext properly? This works for now.

-- | Create a full screen canvas
fullScreenCanvas :: Window -> IO CanvasRenderingContext2D
fullScreenCanvas webView = do
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    setInnerHTML body $ Just canvasHtml
    Just c <- getElementById doc "canvas"
    toContext c
  where canvasHtml :: String
        canvasHtml = "<canvas id=\"canvas\" \
                     \style=\"border:1px \
                     \solid #000000; \
                     \top:0px;bottom:0px;left:0px;right:0px;\">\
                     \</canvas> "

-- | Create a fixed size canvas given the dimensions
fixedSizeCanvas :: Window -> Int -> Int -> IO CanvasRenderingContext2D
fixedSizeCanvas webView x y = do
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    setInnerHTML body (Just $ canvasHtml x y)
    Just c <- getElementById doc "canvas"
    toContext c
  where canvasHtml :: Int -> Int -> String
        canvasHtml x' y' = "<canvas id=\"canvas\" \
                           \width=\""++ show x' ++ "\" \
                           \height=\""++ show y' ++ "\" \
                           \style=\"border:1px \
                           \solid #000000;\">\
                           \</canvas> "

-- | Draws a picture which depends only on the time
animate :: CanvasRenderingContext2D -- ^ the context to draw on
        -> Float -- ^ FPS
        -> (Float -> Picture) -- ^ Your drawing function
        -> IO ()
animate ctx fps f = animateIO ctx fps $ return . f

-- | Draws a picture which depends only on the time... and everything else,
-- since you can do I/O.
animateIO :: CanvasRenderingContext2D -- ^ the context to draw on
          -> Float -- ^ FPS
          -> (Float -> IO Picture) -- ^ Your drawing function
          -> IO ()
animateIO ctx fps f =
    let loop t = do
        stamp <- getCurrentTime
        clearRect ctx (-10000) (-10000) 20000 20000 --FIXME
        setTransform ctx 1 0 0 1 0 0 -- reset transforms (and accumulated errors!).
        pic <- f t
        render ctx pic
        now <- getCurrentTime
        let td = diffUTCTime now stamp
        when (realToFrac td <= 1 / fps) $
          threadDelay $ floor $ (*1000000) (1 / fps - realToFrac td)
        loop (t + 1/fps) --MAYBE change to currentTime - startTime
      in
        loop 0

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

-- | Lets you manage the input.
play :: (IsEventTarget eventElement, IsDocument eventElement)
     => CanvasRenderingContext2D -- ^ the context to draw on
     -> eventElement
     -> Float -- ^ FPS
     -> state -- ^ Initial state
     -> (state -> Picture) -- ^ Drawing function
     -> (Input -> state -> state) -- ^ Input handling function
     -> (Float -> state -> state) -- ^ Stepping function
     -> IO ()
play ctx doc fps initialState draw handleInput step =
  playIO
    ctx
    doc
    fps
    initialState
    (return . draw)
    (\s i -> return $ handleInput s i)
    (\s t -> return $ step s t)

-- | Same thing with I/O
playIO :: (IsEventTarget eventElement, IsDocument eventElement)
       => CanvasRenderingContext2D -- ^ the context to draw on
       -> eventElement
       -> Float -- ^ FPS
       -> state -- ^ Initial state
       -> (state -> IO Picture) -- ^ Drawing function
       -> (Input -> state -> IO state) -- ^ Input handling function
       -> (Float -> state -> IO state) -- ^ Stepping function
       -> IO ()
playIO ctx doc fps initialState draw handleInput step = do
    inputM <- newMVar []
    _ <- on doc mouseDown $ do
        btn <- fmap toMouseBtn mouseButton
        modifiers <- getModifiersMouse
        when (isJust btn) $
          liftIO $ modifyMVar_ inputM $ fmap return (MouseBtn (fromJust btn) Down modifiers :) -- :-) :D XD
    _ <- on doc mouseUp $ do
        btn <- fmap toMouseBtn mouseButton
        modifiers <- getModifiersMouse
        when (isJust btn) $
          liftIO $ modifyMVar_ inputM $ fmap return (MouseBtn (fromJust btn) Up modifiers :) -- :-) :D XD
    _ <- on doc mouseMove $ do
        coords <- mouseOffsetXY -- experimental!
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
        state' <- foldrM handleInput state inputs
        state'' <- step (1/fps) state' --MAYBE change 1/fps to actual td
        clearRect ctx (-10000) (-10000) 20000 20000 --FIXME
        setTransform ctx 1 0 0 1 0 0 -- reset transforms (and accumulated errors!).
        pic <- draw state''
        render ctx pic
        now <- getCurrentTime
        let td = diffUTCTime now stamp
        when (realToFrac td <= 1 / fps) $
          threadDelay $ floor $ (*1000000) (1 / fps - realToFrac td)
        loop state''
      in
        loop initialState
