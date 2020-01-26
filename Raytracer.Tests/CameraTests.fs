module CameraTests

open FsUnit.Xunit
open Xunit
open Raytracer
open Raytracer.Constants

[<Fact>]
let ``Constructing a camera``() =
    let hsize = 160
    let vsize = 120
    let fov = Deg90
    let c = Camera.make hsize vsize fov
    c.HSize |> should equal 160
    c.VSize |> should equal 120
    c.FieldOfView |> should equal Deg90
    c.Transform |> should equal Matrix.identity

[<Fact>]
let ``The pixel size for a horizontal canvas``() =
    let c = Camera.make 200 125 Deg90
    abs (c.PixelSize - 0.01) |> should lessThan Epsilon

[<Fact>]
let ``The pixel size for a vertical canvas``() =
    let c = Camera.make 125 200 Deg90
    abs (c.PixelSize - 0.01) |> should lessThan Epsilon

[<Fact>]
let ``Constructing a ray through the center of the canvas``() =
    let c = Camera.make 201 101 Deg90
    let ray = Camera.rayForPixel c 100 50
    ray.Origin |> should equal Point.origin
    ray.Direction .= (Vector.make 0. 0. -1.0) |> should be True

[<Fact>]
let ``Constructing a ray through a corner of the canvas``() =
    let c = Camera.make 201 101 Deg90
    let ray = Camera.rayForPixel c 0 0
    ray.Origin |> should equal Point.origin
    ray.Direction .= Vector.make 0.66519 0.33259 -0.66851 |> should be True

[<Fact>]
let ``Constructing a ray when the camera is transformed``() =
    let t =
        Transformation.rotationY Deg45
        * Transformation.translation 0. -2.0 5.
    let c = Camera.makeWithTransform 201 101 Deg90 t
    let ray = Camera.rayForPixel c 100 50
    ray.Origin |> should equal (Point.make 0. 2. -5.0)
    ray.Direction .= Vector.make (sqrt 2. / 2.) 0. (-sqrt 2. / 2.)
    |> should be True

[<Fact>]
let ``Rendering the world with a camera``() =
    let w = World.defaultWorld
    let from = Point.make 0. 0. -5.0
    let dest = Point.origin
    let up = Vector.make 0. 1. 0.
    let t = Transformation.viewTransform from dest up
    let c = Camera.makeWithTransform 11 11 Deg90 t
    let image = Camera.render c w
    Canvas.pixelAt image 5 5 .= (Color.make 0.38066 0.47583 0.2855)
    |> should be True
