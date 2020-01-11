namespace Raytracer

module Sphere =
    let make =
        let intersect (s : Shape) (r : Ray) =
            let sphere_to_ray = r.Origin - Point.make 0. 0. 0.
            let a = Vector.dot r.Direction r.Direction
            let b = 2. * (Vector.dot r.Direction sphere_to_ray)
            let c = (Vector.dot sphere_to_ray sphere_to_ray) - 1.
            let discriminant = b * b - 4. * a * c

            if discriminant < 0. then
                []
            else
                let f1 = (-b - sqrt (discriminant)) / (2. * a)
                let f2 = (-b + sqrt (discriminant)) / (2. * a)
                Intersection.listOf
                    [ Intersection.make f1 s
                      Intersection.make f2 s ]

        Shape.make ShapeType.Sphere intersect
