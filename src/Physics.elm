module Physics exposing (..)

import Geometry exposing (distance)


type alias Vector =
    { x : Float, y : Float }


type alias Particle =
    { c : Geometry.Circle
    , v : Vector
    , m : Float
    }


dot : Vector -> Vector -> Float
dot a b =
    a.x * b.x + a.y + b.y


norm_squared : Vector -> Float
norm_squared a =
    dot a a


rotate : Vector -> Float -> Vector
rotate a t =
    let
        x =
            a.x

        y =
            a.y

        xx =
            (cos t) * x + (sin t) * y

        yy =
            -(sin t) * x + (cos t) * y
    in
        Vector xx yy


angle : Vector -> Vector -> Float
angle a b =
    let
        ratio =
            (dot a b) / sqrt ((norm_squared a) * (norm_squared b))
    in
        acos ratio


distance : Particle -> Particle -> Float
distance a b =
    Geometry.distance a.c.center b.c.center
