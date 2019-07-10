module MountainCar where

minPosition = -1.2
maxPosition = 0.6
minInitPos = -0.6
maxInitPos = -0.4
maxSpeed = 0.07
goalPosition = 0.5
force = 0.001
gravity = 0.0025

data Action = ActionLeft | ActionNothing | ActionRight

instance Show Action where
    show ActionLeft = "<"
    show ActionRight = ">"
    show ActionNothing = "="

data Observation = Observation
    { _position :: Double
    , _velocity :: Double }

initObs :: Double -> Observation
initObs xi = Observation (minInitPos + xi * (maxInitPos - minInitPos)) 0

goalObs :: Observation -> Bool
goalObs obs = _position obs >= goalPosition

heightObs :: Double -> Double
heightObs x = 0.55 + 0.45 * sin (3 * x)

stepObs :: Action -> Observation -> Observation
stepObs action obs = obs { _position = p2, _velocity = v3 }
    where dv = case action of ActionNothing -> 0
                              ActionLeft -> -force
                              ActionRight -> force
          v1 = _velocity obs + dv - gravity * cos (3 * _position obs)
          v2 = max (-maxSpeed) $ min maxSpeed v1
          p1 = _position obs + v2
          p2 = max minPosition $ min maxPosition p1
          v3 = if p2 == minPosition && v2 < 0 then 0 else v2

