namespace Raytracer

module Intersection =
    type 'T T =
        { TValue : float
          Object : 'T }

    type 'T IntersectionList = 'T T list

    let make t o =
        { TValue = t
          Object = o }

    let listOf (intersections : 'T IntersectionList) =
        List.sortBy (fun i -> i.TValue) intersections

    let hits (intersections : 'T IntersectionList) =
        List.tryFind (fun i -> i.TValue >= 0.) intersections
