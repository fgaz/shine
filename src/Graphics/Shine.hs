{-|
Module      : Graphics.Shine
Description : Short description
Copyright   : (c) Francesco Gazzetta, 2016
License     : MIT
Maintainer  : francygazz@gmail.com
Stability   : experimental

The main module. Here are defined all the functions needed to get
an animation on the screen.

If you want to render a single 'Picture' only once,
use 'render' from 'Graphics.Shine.Render'
-}
module Graphics.Shine (
  -- * Getting a rendering context
  toContext,
  fullScreenCanvas,
  fixedSizeCanvas,
  -- * Drawing
  animate,
  animateIO,
  play,
  playIO
) where

import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.NonElementParentNode (getElementById)
import GHCJS.DOM.EventM (on, mouseButton, mouseCtrlKey, mouseAltKey, mouseShiftKey, mouseMetaKey, mouseOffsetXY, uiKeyCode, event)
import GHCJS.DOM.GlobalEventHandlers (mouseDown, mouseUp, mouseMove, wheel, keyUp, keyDown)
import GHCJS.DOM.EventTarget (IsEventTarget)
import GHCJS.DOM.WheelEvent (getDeltaX, getDeltaY)
import GHCJS.DOM.KeyboardEvent (KeyboardEvent, getCtrlKey, getShiftKey, getAltKey, getMetaKey)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLCanvasElement (getContext)
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.Types (JSM, Element, MouseEvent, IsDocument, Document)

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
toContext :: Element -- ^ this __must__ be a canvas
          -> JSM CanvasRenderingContext2D
toContext c = do
    Just ctx <- getContext (unsafeCoerce c) "2d" ["2d"]
    return $ unsafeCoerce ctx --how do i get a 2dcontext properly? This works for now.

customAttributesCanvas :: Document -> String -> JSM CanvasRenderingContext2D
customAttributesCanvas doc attrs = do
    Just body <- getBody doc
    setInnerHTML body $ Just canvasHtml
    Just c <- getElementById doc "canvas"
    toContext c
  where canvasHtml :: String
        canvasHtml = "<canvas id=\"canvas\" " ++ attrs ++ " </canvas> "

-- | Create a full screen canvas
fullScreenCanvas :: Document -> JSM CanvasRenderingContext2D
fullScreenCanvas doc = customAttributesCanvas doc attributes
  where attributes :: String
        attributes = "style=\"border:1px \
                     \solid #000000; \
                     \top:0px;bottom:0px;left:0px;right:0px;\""

-- | Create a fixed size canvas given the dimensions
fixedSizeCanvas :: Document -> Int -> Int -> JSM CanvasRenderingContext2D
fixedSizeCanvas doc x y = customAttributesCanvas doc $ attributes x y
  where attributes :: Int -> Int -> String
        attributes x' y' = "width=\""++ show x' ++ "\" \
                           \height=\""++ show y' ++ "\" \
                           \style=\"border:1px \
                           \solid #000000;\""

-- | Draws a picture which depends only on the time
animate :: CanvasRenderingContext2D -- ^ the context to draw on
        -> Double -- ^ FPS
        -> (Double -> Picture) -- ^ Your drawing function
        -> JSM ()
animate ctx fps f = animateIO ctx fps $ return . f

-- | Draws a picture which depends only on the time... and everything else,
-- since you can do I/O.
animateIO :: CanvasRenderingContext2D -- ^ the context to draw on
          -> Double -- ^ FPS
          -> (Double -> IO Picture) -- ^ Your drawing function
          -> JSM ()
animateIO ctx fps f = do
    initialTime <- getCurrentTime
    let loop = do
        stamp <- getCurrentTime
        -- empty the canvas before drawing
        -- MAYBE we need a buffer canvas on which to draw before copying it
        -- on the main canvas (using the bitmaprenderer attribute and
        -- an OffscreenCanvas for efficiency) to avoid blinking
        clearRect ctx (-10000) (-10000) 20000 20000 --FIXME
        setTransform ctx 1 0 0 1 0 0 -- reset transforms (and accumulated errors!).
        -- get the Picture and draw it
        let t = realToFrac $ diffUTCTime stamp initialTime
        pic <- f t
        render ctx pic
        -- delay to match the target fps
        now <- getCurrentTime
        let td = diffUTCTime now stamp
        when (realToFrac td <= 1 / fps) $
          threadDelay $ floor $ (*1000000) (1 / fps - realToFrac td)
        loop
      in
        loop

getModifiersMouse :: ReaderT MouseEvent JSM Modifiers
getModifiersMouse = Modifiers
                   <$> fmap toKeyState mouseCtrlKey
                   <*> fmap toKeyState mouseAltKey
                   <*> fmap toKeyState mouseShiftKey
                   <*> fmap toKeyState mouseMetaKey

getModifiersKeyboard :: ReaderT KeyboardEvent JSM Modifiers
getModifiersKeyboard = Modifiers
                   <$> fmap toKeyState (event >>= getCtrlKey)
                   <*> fmap toKeyState (event >>= getAltKey)
                   <*> fmap toKeyState (event >>= getShiftKey)
                   <*> fmap toKeyState (event >>= getMetaKey)

-- | Lets you manage the input.
play :: (IsEventTarget eventElement, IsDocument eventElement)
     => CanvasRenderingContext2D -- ^ the context to draw on
     -> eventElement
     -> Double -- ^ FPS
     -> state -- ^ Initial state
     -> (state -> Picture) -- ^ Drawing function
     -> (Input -> state -> state) -- ^ Input handling function
     -> (Double -> state -> state) -- ^ Stepping function
     -> JSM ()
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
       -> Double -- ^ FPS
       -> state -- ^ Initial state
       -> (state -> IO Picture) -- ^ Drawing function
       -> (Input -> state -> IO state) -- ^ Input handling function
       -> (Double -> state -> IO state) -- ^ Stepping function
       -> JSM ()
playIO ctx doc fps initialState draw handleInput step = do
    inputM <- newMVar [] -- this will accumulate the inputs
    -- setting up event listeners for mouse and keyboard
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
        liftIO $ modifyMVar_ inputM $ fmap return (Keyboard (keyCodeLookup $ fromIntegral key) Down modifiers :) -- :-) :D XD
    _ <- on doc keyUp $ do
        key <- uiKeyCode
        modifiers <- getModifiersKeyboard
        liftIO $ modifyMVar_ inputM $ fmap return (Keyboard (keyCodeLookup $ fromIntegral key) Up modifiers :) -- :-) :D XD
    initialTime <- getCurrentTime
    -- main loop
    let loop state previousTime = do
        -- retrieve inputs and empty inputM
        inputs <- modifyMVar inputM $ \xs -> return ([], xs)
        -- handle inputs
        state' <- foldrM handleInput state inputs
        -- state stepping
        beforeRendering <- getCurrentTime
        let td = diffUTCTime beforeRendering previousTime
        state'' <- step (realToFrac td) state'
        -- actual rendering begins
        clearRect ctx (-10000) (-10000) 20000 20000 --FIXME
        setTransform ctx 1 0 0 1 0 0 -- reset transforms (and accumulated errors!).
        pic <- draw state''
        render ctx pic
        afterRendering <- getCurrentTime --do we really need two timestamps?
        -- delay to match the target fps
        let renderingTime = diffUTCTime afterRendering beforeRendering
        when (realToFrac renderingTime <= 1 / fps) $
          threadDelay $ floor $ (*1000000) (1 / fps - realToFrac renderingTime)
        loop state'' beforeRendering
      in
        loop initialState initialTime

