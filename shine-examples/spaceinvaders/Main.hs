import Game

import qualified Graphics.Shine as S
import qualified Graphics.Shine.Input as S
import qualified Graphics.Shine.Picture as S
import qualified Web.KeyCode as W

import System.Random (newStdGen, randoms)
import GHCJS.DOM (currentDocumentUnchecked)

displayH :: Game -> S.Picture
displayH g = S.Translate width2 height2 $ case _status g of
    Won -> S.Text "50px Sans" S.CenterAlign Nothing "YOU WIN!"
    Lost -> S.Text "50px Sans" S.CenterAlign Nothing "GAME OVER!"
    _ -> S.Rotate pi $ foldl (<>) paddle (bullets ++ invaders)
    where paddle = drawItem (S.Color 0 127 0 1) (_paddle g)
          bullets = map (drawItem (S.Color 0 0 255 1)) (_bullets g)
          invaders = map (drawItem (S.Color 255 0 0 1)) (_invaders g)
          width2 = fromIntegral $ div gameWidth 2
          height2 = fromIntegral $ div gameHeight 2

drawItem :: S.Color -> Item -> S.Picture
drawItem c it = S.Colored c $ S.Translate x y $ S.RectF sx sy
    where (x, y) = _pos it
          (sx, sy) = _siz it

eventH :: S.Input -> Game -> Game 
eventH (S.Keyboard W.ArrowLeft S.Down _)  g = g { _inputRight = True }
eventH (S.Keyboard W.ArrowLeft S.Up _)    g = g { _inputRight = False }
eventH (S.Keyboard W.ArrowRight S.Down _) g = g { _inputLeft = True }
eventH (S.Keyboard W.ArrowRight S.Up _)   g = g { _inputLeft = False }
eventH (S.Keyboard W.Space S.Down _)      g = g { _inputFire = True }
eventH (S.Keyboard W.Space S.Up _)        g = g { _inputFire = False }
eventH _ g = g

idleH :: Double -> Game -> Game 
idleH = step

main :: IO ()
main = do
    myRands <- randoms <$> newStdGen
    doc <- currentDocumentUnchecked
    ctx <- S.fixedSizeCanvas doc 800 600
    let model0 = createGame myRands
    S.play ctx doc 30 model0 displayH eventH idleH

