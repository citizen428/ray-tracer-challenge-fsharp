namespace Raytracer

open System

module Transformation =
    let translation x y z =
        Matrix.make [ [ 1.; 0.; 0.; x ]
                      [ 0.; 1.; 0.; y ]
                      [ 0.; 0.; 1.; z ]
                      [ 0.; 0.; 0.; 1. ] ]

    let scaling x y z =
        Matrix.make [ [ x; 0.; 0.; 0. ]
                      [ 0.; y; 0.; 0. ]
                      [ 0.; 0.; z; 0. ]
                      [ 0.; 0.; 0.; 1. ] ]

    let rotationX r =
        let cosr = Math.Cos r
        let sinr = Math.Sin r

        Matrix.make [ [ 1.; 0.; 0.; 0. ]
                      [ 0.; cosr; -sinr; 0. ]
                      [ 0.; sinr; cosr; 0. ]
                      [ 0.; 0.; 0.; 1. ] ]

    let rotationY r =
        let cosr = Math.Cos r
        let sinr = Math.Sin r

        Matrix.make [ [ cosr; 0.; sinr; 0. ]
                      [ 0.; 1.; 0.; 0. ]
                      [ -sinr; 0.; cosr; 0. ]
                      [ 0.; 0.; 0.; 1. ] ]

    let rotationZ r =
        let cosr = Math.Cos r
        let sinr = Math.Sin r

        Matrix.make [ [ cosr; -sinr; 0.; 0. ]
                      [ sinr; cosr; 0.; 0. ]
                      [ 0.; 0.; 1.; 0. ]
                      [ 0.; 0.; 0.; 1. ] ]

    let shearing xy xz yx yz zx zy =
        Matrix.make [ [ 1.; xy; xz; 0. ]
                      [ yx; 1.; yz; 0. ]
                      [ zx; zy; 1.; 0. ]
                      [ 0.; 0.; 0.; 1. ] ]

    let viewTransform from dest up =
        let forward = dest - from |> Vector.normalize
        let upNorm = Vector.normalize up
        let left = Vector.cross forward upNorm
        let trueUp = Vector.cross left forward

        let orientation =
            Matrix.make [ [ left.X; left.Y; left.Z; 0. ]
                          [ trueUp.X; trueUp.Y; trueUp.Z; 0. ]
                          [ -forward.X
                            -forward.Y
                            -forward.Z
                            0. ]
                          [ 0.; 0.; 0.; 1. ] ]

        orientation * translation -from.X -from.Y -from.Z
