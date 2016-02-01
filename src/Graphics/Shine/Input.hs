module Graphics.Shine.Input where

import Web.KeyCode

data KeyState = Down | Up deriving (Show, Eq)

data Modifiers = Modifiers { ctrl :: KeyState
                           , alt :: KeyState
                           , shift :: KeyState
                           , meta :: KeyState }
                 deriving (Show, Eq)

data MouseButton = LeftBtn | RightBtn | MiddleBtn deriving (Show, Eq)

data Input = Keyboard Key KeyState Modifiers
           | MouseButton MouseButton KeyState Modifiers
           | MouseWheel (Double, Double)
           | MouseMove (Int, Int)
           deriving (Show, Eq)

toMouseButton :: Word -> Maybe MouseButton
toMouseButton 0 = Just LeftBtn
toMouseButton 1 = Just MiddleBtn
toMouseButton 2 = Just RightBtn
toMouseButton _ = Nothing

toKeyState :: Bool -> KeyState
toKeyState True = Down
toKeyState False = Up
