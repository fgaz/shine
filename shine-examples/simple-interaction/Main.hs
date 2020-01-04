{-# LANGUAGE CPP #-}

import Graphics.Shine
import Graphics.Shine.Input
import Graphics.Shine.Picture

import GHCJS.DOM (currentDocumentUnchecked)

#if defined(ghcjs_HOST_OS)
run :: a -> a
run = id
#elif defined(MIN_VERSION_jsaddle_wkwebview)
import Language.Javascript.JSaddle.WKWebView (run)
#else
import Language.Javascript.JSaddle.WebKitGTK (run)
#endif

main :: IO ()
main = run $ do
    doc <- currentDocumentUnchecked
    ctx <- fixedSizeCanvas doc 800 600
    play ctx doc 30 initialState draw handleInput step
  where
    initialState = False
    draw False = Empty
    draw True = RectF 300 300
    handleInput (MouseBtn BtnLeft Down _) = const True
    handleInput (MouseBtn BtnLeft Up _) = const False
    handleInput _ = id
    step _ = id
