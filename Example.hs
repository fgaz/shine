import Shine

myPic :: Float -> Picture
myPic x = RectF 0 0 (150+x') 30 
       <> (Colored (Color 255 0 0 1.0) $ Rect 0 0 30 (150+x'))
       <> (Colored (Color 200 200 0 1.0) $ RectF 600 0 120 60)
       <> (Colored (Color 100 100 (floor x') 1.0) $ RectF 600 340 120 240)
       <> circle (150+x') 150 100
       <> circle 140 120 (80+x')
       <> Line 400 400 10 x'
       <> (Colored (Color 255 0 0 1.0) $ CircleF 800 500 (x'/10))
       <> (foldMap (circle 300 300) [1,5..x'])
    where x' = sin (x/300) *100 +100

main :: IO ()
main = animate (800,600) $ myPic
