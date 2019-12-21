namespace Raytracer.Types

open Raytracer.Constants

module rec Tuples =
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
                abs (p1.X - p2.X) < epsilon && abs (p1.Y - p2.Y) < epsilon
                && abs (p1.Z - p2.Z) < epsilon && abs (p1.W - p2.W) < epsilon

        let make x y z =
            { X = x
              Y = y
              Z = z
              W = 1.0 }

    module Vector =
        type T =
            { X : float
              Y : float
              Z : float
              W : float }

            static member (+) (v1, v2) =
                { X = v1.X + v2.X
                  Y = v1.Y + v2.Y
                  Z = v1.Z + v2.Z
                  W = v1.W + v2.W }

            static member (-) (v1, v2) =
                { X = v1.X - v2.X
                  Y = v1.Y - v2.Y
                  Z = v1.Z - v2.Z
                  W = v1.W - v2.W }

            static member (~-) (v) =
                { X = -v.X
                  Y = -v.Y
                  Z = -v.Z
                  W = -v.W }

            static member (*) (v, scalar) =
                make (v.X * scalar) (v.Y * scalar) (v.Z * scalar)

            static member (/) (v, scalar) =
                { X = v.X / scalar
                  Y = v.Y / scalar
                  Z = v.Z / scalar
                  W = v.W / scalar }

            static member (.=) (v1, v2) =
                abs (v1.X - v2.X) < epsilon && abs (v1.Y - v2.Y) < epsilon
                && abs (v1.Z - v2.Z) < epsilon && abs (v1.W - v2.W) < epsilon

        let make x y z =
            { X = x
              Y = y
              Z = z
              W = 0.0 }

        let magnitude v = sqrt (v.X * v.X + v.Y * v.Y + v.Z * v.Z)

        let normalize v =
            let mag = magnitude v
            make (v.X / mag) (v.Y / mag) (v.Z / mag)

        let dot v1 v2 = v1.X * v2.X + v1.Y * v2.Y + v1.Z * v2.Z

        let cross v1 v2 =
            make (v1.Y * v2.Z - v1.Z * v2.Y) (v1.Z * v2.X - v1.X * v2.Z)
                (v1.X * v2.Y - v1.Y * v2.X)
