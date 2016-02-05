module Graphics.Shine (
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
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.Types (Window, Document, MouseEvent)

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
        render ctx pic
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
