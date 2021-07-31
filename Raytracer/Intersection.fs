namespace Raytracer

type 'T Intersection = { TValue: float; Object: 'T }

module Intersection =
    let make t o = { TValue = t; Object = o }

    let listOf (intersections: 'T Intersection list) =
        List.sortBy (fun i -> i.TValue) intersections

    let hits (intersections: 'T Intersection list) =
        List.tryFind (fun i -> i.TValue >= 0.) intersections
