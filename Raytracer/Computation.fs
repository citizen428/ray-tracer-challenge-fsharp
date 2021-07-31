namespace Raytracer

type 'T Computation =
    { TValue: float
      Object: 'T
      Point: Point
      OverPoint: Point
      EyeV: Vector
      NormalV: Vector
      Inside: bool
      ReflectV: Vector }

module Computation =
    let prepare (i: Shape Intersection) r =
        let point = Ray.position r i.TValue
        let eyev = -r.Direction
        let normalv = Shape.normalAt i.Object point
        let inside = Vector.dot normalv eyev < 0.

        let actualNormalv = if inside then -normalv else normalv

        let overPoint =
            point + actualNormalv * Constants.Epsilon

        let reflectV = Vector.reflect r.Direction actualNormalv

        { TValue = i.TValue
          Object = i.Object
          Point = point
          OverPoint = overPoint
          EyeV = eyev
          NormalV = actualNormalv
          Inside = inside
          ReflectV = reflectV }
