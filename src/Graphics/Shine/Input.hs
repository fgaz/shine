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
data MouseButton = LeftBtn | RightBtn | MiddleBtn deriving (Show, Eq)

-- | Datatype representing all possible inputs
data Input = Keyboard Key KeyState Modifiers
           | MouseButton MouseButton KeyState Modifiers
           | MouseWheel (Double, Double)
           | MouseMove (Int, Int)
           deriving (Show, Eq)

-- | Convert a js mouse button identifier to the corresponding datatype
toMouseButton :: Word -> Maybe MouseButton
toMouseButton 0 = Just LeftBtn
toMouseButton 1 = Just MiddleBtn
toMouseButton 2 = Just RightBtn
toMouseButton _ = Nothing

-- | Convert a bool (from js) to a keystate
toKeyState :: Bool -> KeyState
toKeyState True = Down
toKeyState False = Up
