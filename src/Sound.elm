module Sound exposing (play, currentTime)

import Native.Sound

currentTime : () -> Float
currentTime = Native.Sound.currentTime

play : Float -> Float -> Float -> Float -> ()
play = Native.Sound.play