#r "../../Raytracer/obj/Debug/net5.0/Raytracer.dll"

open Raytracer
open System.IO

let scene =
    let floorMaterial =
        Material.make
        |> Material.withSpecular 0.
        |> Material.withPattern (Pattern.checkers Color.white Color.black)

    let floor = Shape.texture Plane.make floorMaterial

    let pattern =
        Pattern.transform (Pattern.stripe Color.red Color.white) (Transformation.scaling 0.2 0.2 0.2)

    let sphereMaterial =
        Material.make |> Material.withPattern pattern

    let sphere =
        Shape.transform Sphere.make (Transformation.translation 0. 1. 2.)

    let texturedSphere = Shape.texture sphere sphereMaterial

    let tx = Transformation.translation 0. 0. 5.
    let rx = Transformation.rotationX Constants.Deg90
    let sx = Transformation.scaling 10. 0.01 10.
    let t = tx * rx * sx
    let plane = Shape.transform Plane.make t

    let planePattern =
        Pattern.transform (Pattern.ring Color.red Color.black) (Transformation.scaling 0.2 0.2 0.2)

    let planeMaterial =
        Material.make |> Material.withPattern planePattern

    let wall = Shape.texture plane planeMaterial

    let light =
        Light.make (Point.make -10.0 10. -10.0) (Color.make 1. 1. 1.)

    let shapes = [ floor; texturedSphere; wall ]

    World.make shapes [ light ]

let view =
    Transformation.viewTransform (Point.make 0. 1.5 -5.0) (Point.make 0. 1. 0.) (Vector.make 0. 1. 0.)

let camera =
    Camera.makeWithTransform 1000 500 (System.Math.PI / 3.) view

let canvas = Camera.render camera scene

File.WriteAllText("img/ch10-patterns.ppm", Canvas.toPpm canvas)
