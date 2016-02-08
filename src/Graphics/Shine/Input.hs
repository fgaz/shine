module Graphics.Shine.Input where

import Web.KeyCode

-- | The state of a button on the keyboard
data KeyState = Down | Up deriving (Show, Eq)

-- | The four key modifiers
data Modifiers = Modifiers { ctrl :: KeyState
                           , alt :: KeyState
                           , shift :: KeyState
                           , meta :: KeyState }
                 deriving (Show, Eq)

-- | The three mouse buttons
data MouseBtn = BtnLeft | BtnRight | BtnMiddle deriving (Show, Eq)

-- | Datatype representing all possible inputs
data Input = Keyboard Key KeyState Modifiers
           | MouseBtn MouseBtn KeyState Modifiers
           | MouseWheel (Double, Double)
           | MouseMove (Int, Int)
           deriving (Show, Eq)

-- | Convert a js mouse button identifier to the corresponding datatype
toMouseBtn :: Word -> Maybe MouseBtn
toMouseBtn 0 = Just BtnLeft
toMouseBtn 1 = Just BtnMiddle
toMouseBtn 2 = Just BtnRight
toMouseBtn _ = Nothing

-- | Convert a bool (from js) to a keystate
toKeyState :: Bool -> KeyState
toKeyState True = Down
toKeyState False = Up
