import Graphics.Shine
import Graphics.Shine.Input
import Graphics.Shine.Picture

import GHCJS.DOM (webViewGetDomDocument, runWebGUI)

main :: IO ()
main = runWebGUI $ \ webView -> do
    ctx <- fixedSizeCanvas webView 800 600
    Just doc <- webViewGetDomDocument webView
    play ctx doc 30 initialState draw handleInput step
  where
    initialState = False
    draw False = Empty
    draw True = RectF 300 300
    handleInput (MouseBtn BtnLeft Down _) = const True
    handleInput (MouseBtn BtnLeft Up _) = const False
    handleInput _ = id
    step _ = id
