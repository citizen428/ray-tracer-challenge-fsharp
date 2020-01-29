#r "../../Raytracer/obj/Debug/netcoreapp3.1/Raytracer.dll"

open Raytracer
open System.IO

let scene =
    let plainFloor =
        Shape.transform Sphere.make (Transformation.scaling 10. 0.01 10.)

    let floorMaterial =
        Material.make
        |> Material.withColor (Color.make 1. 0.9 0.9)
        |> Material.withSpecular 0.

    let floor = Shape.texture plainFloor floorMaterial

    let tx = Transformation.translation 0. 0. 5.
    let ry = Transformation.rotationY (-Constants.Deg45)
    let rx = Transformation.rotationX Constants.Deg90
    let sx = Transformation.scaling 10. 0.01 10.
    let plainLeftWall =
        Shape.transform Sphere.make (List.fold (*) tx [ ry; rx; sx ])
    let leftWall = Shape.texture plainLeftWall floorMaterial

    let tx = Transformation.translation 0. 0. 5.
    let ry = Transformation.rotationY Constants.Deg45
    let rx = Transformation.rotationX Constants.Deg90
    let sx = Transformation.scaling 10. 0.01 10.
    let plainRightWall =
        Shape.transform Sphere.make (List.fold (*) tx [ ry; rx; sx ])
    let rightWall = Shape.texture plainRightWall floorMaterial

    let plainMiddle =
        Shape.transform Sphere.make (Transformation.translation -0.5 1. 0.5)

    let middleMaterial =
        Material.make
        |> Material.withColor (Color.make 0.1 1. 0.5)
        |> Material.withDiffuse 0.7
        |> Material.withSpecular 0.3

    let middle = Shape.texture plainMiddle middleMaterial

   
    let tx = Transformation.translation 1.5 0.5 -0.5
    let sx = Transformation.scaling 0.5 0.5 0.5
    let plainRight = Shape.transform Sphere.make (tx * sx)
    let rightMaterial =
        Material.withColor (Color.make 0.5 1. 0.1) middleMaterial
    let right = Shape.texture plainRight rightMaterial


    let tx = Transformation.translation -1.5 0.33 -0.75
    let sx = Transformation.scaling 0.33 0.33 0.33
    let plainLeft = Shape.transform Sphere.make (tx * sx)
    let leftMaterial =
        Material.withColor (Color.make 1. 0.8 0.1) middleMaterial
    let left = Shape.texture plainLeft leftMaterial


    let light =
        Light.make (Point.make -10.0 10. -10.0) (Color.make 1. 1. 1.)

    let shapes = [ floor; leftWall; rightWall; middle; right; left ]

    World.make shapes [ light ]

let view =
    Transformation.viewTransform (Point.make 0. 1.5 -5.0)
        (Point.make 0. 1. 0.) (Vector.make 0. 1. 0.)
let camera = Camera.makeWithTransform 1000 500 (System.Math.PI / 3.) view

let canvas = Camera.render camera scene

File.WriteAllText("img/ch07-scene.ppm", Canvas.toPpm canvas)
