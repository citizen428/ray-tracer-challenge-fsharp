namespace Raytracer

module Plane =
    let make =
        let intersect (s : Shape) (r : Ray) =
            if (abs r.Direction.Y) < Constants.Epsilon then
                []
            else
                let t = -r.Origin.Y / r.Direction.Y
                Intersection.listOf [ Intersection.make t s ]

        // All planes are xz unless translated
        let normalAt (s : Shape) (p : Point) = Vector.make 0. 1. 0.

        Shape.make ShapeType.Plane intersect normalAt
