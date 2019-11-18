import qualified Game as G
import qualified MountainCar as MC
import qualified Point as P

import qualified Graphics.Shine as S
import qualified Graphics.Shine.Input as S
import qualified Graphics.Shine.Picture as S
import qualified Web.KeyCode as W

import System.Random (newStdGen, randoms)
import GHCJS.DOM (currentDocumentUnchecked)

gameWidth = 800 :: Double
gameHeight = 600 :: Double
fps = 30
minSpaceX = MC.minPosition - 0.1
maxSpaceX = MC.maxPosition + 0.1
minSpaceY = 0
maxSpaceY = 1.1
goalSpaceX = 0.5
goalSpaceY = MC.heightObs goalSpaceX

displayH :: G.Game -> S.Picture
displayH g = goalP <> carP <> episodeP <> nwinsP <> stepP <> actionP <> groundP
    where carP = mkCarP $ G._observation g
          episodeP = S.Translate 10 30 $ S.Text "20px Sans" S.LeftAlign Nothing $ "episode " ++ show (G._episode g)
          nwinsP = S.Translate 10 60 $ S.Text "20px Sans" S.LeftAlign Nothing $ "nwins " ++ show (G._nwins g)
          stepP = S.Translate 10 90 $ S.Text "20px Sans" S.LeftAlign Nothing $ "step " ++ show (G._step g) ++ " / " ++ show G.maxStep
          actionP = S.Translate 10 120 $ S.Text "20px Sans" S.LeftAlign Nothing $ "action " ++ show (G._lastAction g)

mkCarP :: MC.Observation -> S.Picture
mkCarP obs = S.Translate x y $ S.Colored (S.Color 0 0 255 1) $ S.CircleF 10
    where pos = MC._position obs
          (x,y) = spaceToScreen (pos, MC.heightObs pos)

goalP :: S.Picture
goalP = S.Translate x y $ S.Colored (S.Color 255 0 0 1) $ S.RectF 10 20
    where (x,y) = spaceToScreen (goalSpaceX, goalSpaceY)

groundP :: S.Picture
groundP = foldl (<>) S.Empty [S.Line x0 y0 x1 y1 | ((x0,y0), (x1,y1)) <- zip xys (tail xys)]
    where xys = [spaceToScreen (x, MC.heightObs x) 
                    | x<-[MC.minPosition, MC.minPosition+0.05 .. MC.maxPosition]]

spaceToScreen :: P.Point -> P.Point
spaceToScreen (x, y) = (xScreen, yScreen)
    where xScreen = gameWidth * (x - minSpaceX) / (maxSpaceX - minSpaceX)
          yScreen = gameHeight * (1 - (y - minSpaceY) / (maxSpaceY - minSpaceY))

eventH :: S.Input -> G.Game -> G.Game 
eventH (S.Keyboard W.ArrowLeft S.Down _)  g = g { G._inputLeft = True }
eventH (S.Keyboard W.ArrowLeft S.Up _)    g = g { G._inputLeft = False }
eventH (S.Keyboard W.ArrowRight S.Down _) g = g { G._inputRight = True }
eventH (S.Keyboard W.ArrowRight S.Up _)   g = g { G._inputRight = False }
eventH _ g = g

idleH :: Double -> G.Game -> G.Game
idleH _ = G.step

main :: IO ()
main = do
    myRands <- randoms <$> newStdGen
    doc <- currentDocumentUnchecked
    ctx <- S.fixedSizeCanvas doc (floor gameWidth) (floor gameHeight)
    let myModel = G.createGame myRands
    S.play ctx doc fps myModel displayH eventH idleH

