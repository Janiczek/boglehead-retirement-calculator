module Utils exposing (round100)


round100 : Float -> Float
round100 n =
    toFloat (round (n * 100)) / 100
