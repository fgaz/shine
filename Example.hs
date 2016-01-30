import Shine

myPic :: Float -> Picture
myPic x = Translate (75+x'/2) 15 (RectF (150+x') 30)
       <> (Colored (Color 255 0 0 1.0) $ Translate 15 (75+x'/2) $ Rect 30 (150+x'))
       <> (Colored (Color 200 200 0 1.0) $ Translate 660 30 $ RectF 120 60)
       <> (Colored (Color 100 100 (floor x') 1.0) $ Translate 600 340 $ RectF 120 240)
       <> Translate (150+x') 150 (circle 100)
       <> Translate 140 120 (circle (80+x'))
       <> Line 400 400 10 x'
       <> (Colored (Color 255 0 0 1.0) $ Translate 800 500 $ CircleF (x'/10))
       <> (foldMap (Translate 300 300 . circle) [1,5..x'])
       <> (Translate 350 350 $ Rotate (x'/200) $ RectF 150 30)
    where x' = sin (x/300) *100 +100

main :: IO ()
main = animate (800,600) $ myPic
