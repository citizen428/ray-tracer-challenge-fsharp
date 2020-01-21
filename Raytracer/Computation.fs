namespace Raytracer

type 'T Computation =
    { TValue : float
      Object : 'T
      Point : Point
      EyeV : Vector
      NormalV : Vector
      Inside : bool }

module Computation =
    let prepare (i : Shape Intersection) r =
        let point = Ray.position r i.TValue
        let eyev = -r.Direction
        let normalv = Shape.normalAt i.Object point
        let inside = Vector.dot normalv eyev < 0.

        let actualNormalv =
            if inside then -normalv else normalv

        { TValue = i.TValue
          Object = i.Object
          Point = point
          EyeV = eyev
          NormalV = actualNormalv
          Inside = inside }
