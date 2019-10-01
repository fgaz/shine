module Game where

import qualified MountainCar as MC

maxStep :: Int
maxStep = 200

data Game = Game
    { _observation :: MC.Observation
    , _lastAction :: MC.Action
    , _nwins :: Int
    , _episode :: Int
    , _step :: Int
    , _xis :: [Double]
    , _inputLeft :: Bool
    , _inputRight :: Bool }

createGame :: [Double] -> Game
createGame xis = Game (MC.initObs $ head xis) MC.ActionNothing 0 0 0 (tail xis) False False

step :: Game -> Game
step (Game obs _ nw eps ns xis il ir)
    | ns >= maxStep = Game (MC.initObs $ head xis) MC.ActionNothing nw (eps+1) 0 (tail xis) il ir
    | MC.goalObs obs = Game (MC.initObs $ head xis) MC.ActionNothing (nw+1) (eps+1) 0 (tail xis) il ir
    | otherwise = Game obs' action' nw eps (ns+1) xis il ir
    where action' | il && not ir = MC.ActionLeft
                  | not il && ir = MC.ActionRight
                  | otherwise = MC.ActionNothing
          obs' = MC.stepObs action' obs

