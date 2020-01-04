module Game where

import qualified Data.Map as M
import qualified Point as P

gameWidth, gameHeight :: Int
gameWidth = 800
gameHeight = 600

data Status = Running | Won | Lost deriving (Eq)

data Item = Item
    { _siz :: P.Point
    , _pos :: P.Point
    , _vel :: P.Point }

data Game = Game
    { _status :: Status
    , _inputLeft :: Bool
    , _inputRight :: Bool
    , _inputFire :: Bool
    , _rands :: [Double]
    , _firetime :: Double
    , _paddle :: Item
    , _bullets :: [Item]
    , _invaders :: [Item] }

createGame :: [Double] -> Game
createGame rands0 = Game Running False False False rands1 0 myPaddle [] myInvaders
    where myPaddle = Item (70, 20) (0, -250) (0, 0)
          ([mag, dir], rands1) = splitAt 2 rands0
          vx = (150 + 200 * mag) * (if dir < 0.5 then 1 else -1)
          myInvaders = [ Item (70, 20) 
                              (fromIntegral x * 100, fromIntegral y * 50 + 150)
                              (vx, 0)
                         | x<-[-2..(2::Int)], y<-[0..(2::Int)] ]

updatePaddle :: Double -> Game -> Game
updatePaddle time g = firePaddleBullet time g1
    where dx = time * 200
          dl = if _inputLeft g then -dx else 0
          dr = if _inputRight g then dx else 0
          p0 = _paddle g
          (x0, y) = _pos p0
          x1 = max (-400) $ min 400 $ dl + dr + x0
          p1 = p0 { _pos = (x1, y) }
          g1 = g { _paddle = p1 }

firePaddleBullet ::  Double -> Game -> Game
firePaddleBullet time g = if canFire then mFire else mNofire
    where canFire = _inputFire g && _firetime g > 0.9
          (x, y) = _pos $ _paddle g
          bullet = Item (3, 9) (x, y+20) (0, 200)
          mFire = g { _bullets = bullet : _bullets g, _firetime = 0 }
          mNofire = g { _firetime = time + _firetime g }

updateInvaders :: Double -> Game -> Game
updateInvaders time g = if null myInvaders then g else g3
    where myInvaders = _invaders g
          i1 = map (autoUpdateItem time) myInvaders
          xs = map (fst . _pos) myInvaders
          x1min = minimum xs
          x1max = maximum xs
          v@(vx, _) = _vel $ head myInvaders
          move v0 v1 i = i { _pos = _pos i P.+ time P.* v0, _vel = v1 }
          i2 | vx>0 && x1max>380 = map (move (380-x1max, 0) (P.negate v)) i1
             | vx<0 && x1min<(-380) = map (move (-380-x1min, 0) (P.negate v)) i1
             | otherwise = i1
          g2 = g { _invaders = i2 }
          g3 = fireInvadersBullets g2

fireInvadersBullets :: Game -> Game
fireInvadersBullets g = g { _bullets = _bullets g ++ bs, _rands = rands3 }
    where invadersPos = map _pos $ _invaders g
          fInsert pMap (x,y) = M.insertWith min x y pMap
          fighters0 = M.toList $ foldl fInsert M.empty invadersPos
          (rands0, rands1) = splitAt (length fighters0) (_rands g)
          (rands2, rands3) = splitAt (length fighters0) rands1
          difficulty = 0.9 + fromIntegral (length invadersPos) * (0.99 - 0.9) / 15
          fighters1 = [ (p, v) | (p, r, v) <- zip3 fighters0 rands0 rands2, r > difficulty ]
          createBullet ((x, y), v) = Item (3, 9) (x, y-20) (0, -(300-v*200))
          bs = map createBullet fighters1

autoUpdateItem :: Double -> Item -> Item
autoUpdateItem t i@(Item _ pos vel) = i { _pos = pos P.+ t P.* vel }

updateBullets :: Double -> Game -> Game
updateBullets time g = g { _bullets = b2 }
    where b1 = map (autoUpdateItem time) (_bullets g)
          b2 = filter (\b -> snd (_pos b) < 300 && snd (_pos b) > -300) b1

updateCollisions :: Game -> Game
updateCollisions g = g1 { _invaders = i1, _bullets = b2, _status = st }
    where (b1, i1) = runCollisions (_bullets g) (_invaders g)
          (b2, p2) = runCollisions b1 [_paddle g]
          st | null i1 = Won 
             | null p2 = Lost
             | otherwise = Running
          g1 = if st == Running then g 
               else g { _inputLeft = False, _inputRight = False, _inputFire = False }

testCollision :: Item -> Item -> Bool
testCollision (Item as ap _) (Item bs bp _) =
    ((bx0 < ax0 && ax0 < bx1) || (bx0 < ax1 && ax1 < bx1)) &&
    ((by0 < ay0 && ay0 < by1) || (by0 < ay1 && ay1 < by1))
    where (ax0, ay0) = ap P.- 0.5 P.* as
          (ax1, ay1) = ap P.+ 0.5 P.* as
          (bx0, by0) = bp P.- 0.5 P.* bs
          (bx1, by1) = bp P.+ 0.5 P.* bs

runCollisions :: [Item] -> [Item] -> ([Item], [Item])
runCollisions [] is = ([], is)
runCollisions (b:bs) is = (bs1++bs2, is2)
    where is1 = filter (not . testCollision b) is
          bs1 = [b | length is1 == length is]
          (bs2, is2) = runCollisions bs is1

step :: Double -> Game -> Game
step time g = if _status g == Running then mRunning else mEnd
    where mRunning = updatePaddle time
                        $ updateInvaders time 
                        $ updateBullets time
                        $ updateCollisions g
          mEnd = if _inputFire g then createGame (_rands g) else g

