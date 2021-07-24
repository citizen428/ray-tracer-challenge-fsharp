#r "../../Raytracer/obj/Debug/net5.0/Raytracer.dll"

open Raytracer
open System.IO

let scene =
    let floorMaterial =
        Material.make
        |> Material.withColor (Color.make 1. 0.9 0.9)
        |> Material.withSpecular 0.

    let floor = Shape.texture Plane.make floorMaterial

    let plainMiddle =
        Shape.transform Sphere.make (Transformation.translation -0.5 1. 0.5)

    let sphereMaterial =
        Material.make
        |> Material.withColor (Color.make 0.1 1. 0.5)
        |> Material.withDiffuse 0.7
        |> Material.withSpecular 0.3

    let sphere = Shape.texture Sphere.make sphereMaterial

    let light =
        Light.make (Point.make -10.0 10. -10.0) (Color.make 1. 1. 1.)

    let shapes = [ floor; sphere ]

    World.make shapes [ light ]

let view =
    Transformation.viewTransform (Point.make 0. 1.5 -5.0)
        (Point.make 0. 1. 0.) (Vector.make 0. 1. 0.)
let camera = Camera.makeWithTransform 1000 500 (System.Math.PI / 3.) view

let canvas = Camera.render camera scene

File.WriteAllText("img/ch09-plane.ppm", Canvas.toPpm canvas)
