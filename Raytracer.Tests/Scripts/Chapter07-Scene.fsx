#r "../../Raytracer/obj/Debug/netcoreapp3.1/Raytracer.dll"

open Raytracer

let scene =
  let plainFloor = Shape.transform Sphere.make (Transformation.scaling 10. 0.01 10.)
  let floorMaterial = 
    Material.make 
    |> Material.withColor (Color.make 1. 0.9 0.9) 
    |> Material.withSpecular 0.
  let floor = Shape.texture plainFloor floorMaterial

  let tx = Transformation.translation 0. 0. 5.
  let ry = Transformation.rotatationY (-Constants.Deg45)
  let rx = Transformation.rotatationX Constants.Deg90
  let sx = Transformation.scaling 10. 0.01 10.
  let plainLeftWall = Shape.transform Sphere.make (List.fold (*) tx [ry; rx; sx])
  let leftWall = Shape.texture plainLeftWall floorMaterial

  let tx = Transformation.translation 0. 0. 5.
  let ry = Transformation.rotatationY qtr_pi
  let rx = Transformation.rotatationX half_pi
  let sx = Transformation.scaling 10. 0.01 10.
  let plainRightWall = Shape.transform Sphere.make (List.fold (*) tx [ry; rx; sx])
  let rightWall = Shape.texture plainRightWall floorMaterial

  let plainMiddle = Shape.transform Sphere.make (Transformation.translation -0.5 1. 0.5)
  let middleMaterial =
      Material.make
      |> Material.withColor (Color.make 0.1 1. 0.5)
      |> Material.withDiffuse 0.7
      |> Material.withSpecular 0.3
  let middle = Shape.texture plainMiddle middleMaterial
 

  let tx = Transformation.translation 1.5 0.5 (-0.5)
  let sx = Transformation.scaling 0.5 0.5 0.5
  let right' = Shape.transform (Sphere.make ()) (Matrix.mult tx sx)
  let right = Shape.texture right' (Material.make ~color:(Color.make 0.5 1. 0.1)
                                        ~diffuse:0.7
                                        ~specular:0.3
                                        ())
 

  let tx = Transformation.translation (-1.5) 0.33 (-0.75)
  let sx = Transformation.scaling 0.33 0.33 0.33
  let left' = Shape.transform (Sphere.make ()) (Matrix.mult tx sx)
  let left = Shape.texture left' (Material.make ~color:(Color.make 1. 0.8 0.1)
                                        ~diffuse:0.7
                                        ~specular:0.3
                                        ())
 

  let light = Light.point (Tuple.point (-10.) 10. (-10.)) (Color.make 1. 1. 1.)

  let shapes = [floor; leftWall; rigthWall; middle; right; left]

  World.make shapes [light]

let view = Transformation.viewTransform (Point.make 0. 1.5 -5.) (Point.make 0. 1. 0.) (Vector.make 0. 1. 0.)
let camera = Camera.makeWithTransform 600 300 (System.Math.PI /. 3.) view

let image = Camera.render camera scene
let ppm = Canvas.toPpm image
let f = open_out "07-scene.ppm"
output_string f ppm;
Printf.printf "wrote `07-scene.ppm'\n"
