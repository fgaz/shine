{-# LANGUAGE CPP #-}

import Graphics.Shine
import Graphics.Shine.Image
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

animation :: ImageData -> Double -> Picture
animation img x =
    Translate (75+x'/2) 15 (RectF (150+x') 30)
    <> Colored (Color 255 0 0 1.0) (Translate 15 (75+x'/2) $ Rect 30 (150+x'))
    <> Colored (Color 200 200 0 1.0) (Translate 660 30 $ RectF 120 60)
    <> Colored (Color 100 100 (floor x') 1.0) (Translate 600 340 $ RectF 120 240)
    <> Translate (150+x') 150 (circle 100)
    <> Translate 140 120 (circle (80+x'))
    <> Line 400 400 10 x'
    <> Colored (Color 255 0 0 1.0) (Translate 800 500 $ CircleF (x'/10))
    <> foldMap (Translate 300 300 . circle) [1,5..x']
    <> Translate 350 350 (Rotate (x'/200) $ RectF 150 30)
    <> Translate 100 500 (Image Original img)
    <> Colored (Color 0 0 255 1) -- blue pentagon
           (Translate 200 500
               (Polygon [ (-110,-80)
                        , (50,-120)
                        , (140,20)
                        , (30,140)
                        , (-120,80)
                        ]))
    <> Translate 600 500 (Text "20px Sans" CenterAlign (Just 300) "The quick brown fox jumps over the lazy dog")
  where x' = sin (x*3) *100 +100

main :: IO ()
main = run $ do
    doc <- currentDocumentUnchecked
    ctx <- fixedSizeCanvas doc 800 600
    img <- makeImage "httpS://placehold.it/200x70/afa"
    animate ctx 30 $ animation img
