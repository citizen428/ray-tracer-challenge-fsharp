namespace Raytracer

open Constants

module Point =
    type T =
        { X : float
          Y : float
          Z : float
          W : float }

        static member (+) (p, v : Vector.T) =
            { X = p.X + v.X
              Y = p.Y + v.Y
              Z = p.Z + v.Z
              W = p.W + v.W }

        static member (-) (p, v : Vector.T) =
            { X = p.X - v.X
              Y = p.Y - v.Y
              Z = p.Z - v.Z
              W = p.W - v.W }

        static member (-) (p1, p2) : Vector.T =
            { X = p1.X - p2.X
              Y = p1.Y - p2.Y
              Z = p1.Z - p2.Z
              W = p1.W - p2.W }

        static member (.=) (p1, p2) =
            abs (p1.X - p2.X) < Epsilon && abs (p1.Y - p2.Y) < Epsilon
            && abs (p1.Z - p2.Z) < Epsilon && abs (p1.W - p2.W) < Epsilon

    let make x y z =
        { X = x
          Y = y
          Z = z
          W = 1.0 }