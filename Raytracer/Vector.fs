namespace Raytracer

open Constants

type Vector =
    { X: float
      Y: float
      Z: float
      W: float }

    static member (+)(v1, v2) =
        { X = v1.X + v2.X
          Y = v1.Y + v2.Y
          Z = v1.Z + v2.Z
          W = v1.W + v2.W }


    static member (-)(v1, v2) =
        { X = v1.X - v2.X
          Y = v1.Y - v2.Y
          Z = v1.Z - v2.Z
          W = v1.W - v2.W }


    static member (~-)(v) =
        { X = -v.X
          Y = -v.Y
          Z = -v.Z
          W = -v.W }


    static member (*)(v, scalar) =
        { X = v.X * scalar
          Y = v.Y * scalar
          Z = v.Z * scalar
          W = v.W * scalar }


    static member (/)(v, scalar) =
        { X = v.X / scalar
          Y = v.Y / scalar
          Z = v.Z / scalar
          W = v.W / scalar }

    static member (.=)(v1, v2) =
        abs (v1.X - v2.X) < Epsilon
        && abs (v1.Y - v2.Y) < Epsilon
        && abs (v1.Z - v2.Z) < Epsilon
        && abs (v1.W - v2.W) < Epsilon

module Vector =
    let make x y z = { X = x; Y = y; Z = z; W = 0.0 }

    let magnitude v =
        sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)

    let normalize v =
        let mag = magnitude v
        make (v.X / mag) (v.Y / mag) (v.Z / mag)

    let dot v1 v2 = v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

    let cross v1 v2 =
        make (v1.Y * v2.Z - v1.Z * v2.Y) (v1.Z * v2.X - v1.X * v2.Z) (v1.X * v2.Y - v1.Y * v2.X)

    let reflect v n = v - n * 2. * dot v n
