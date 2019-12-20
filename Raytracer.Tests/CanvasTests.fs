module CanvasTests

open FsUnit.Xunit
open Xunit
open Raytracer.Types
open TestUtilities

[<Fact>]
let ``creating a canvas``() =
    let canvas = Canvas.make 10 20
    canvas.Width |> should equal 10
    canvas.Height |> should equal 20
    canvas.Pixels |> should equal (Array2D.create 10 20 Color.black)

[<Fact>]
let ``writing a pixel to a canvas``() =
    let canvas = Canvas.make 10 20
    Canvas.writePixel canvas 2 3 Color.red
    Canvas.pixelAt canvas 2 3 |> should equal Color.red

[<Fact>]
let ``Constructing the PPM header``() =
    let c = Canvas.make 5 3
    let ppm = Canvas.toPpm c |> lines

    ppm.[0] |> should equal "P3"
    ppm.[1] |> should equal "5 3"
    ppm.[2] |> should equal "255"

[<Fact>]
let ``Constructing the PPM pixel data``() =
    let c = Canvas.make 5 3
    let c1 = Color.make 1.5 0.0 0.0 
    let c2 = Color.make 0.0 0.5 0.0
    let c3 = Color.make -0.5 0.0 1.0
    Canvas.writePixel c 0 0 c1
    Canvas.writePixel c 2 1 c2
    Canvas.writePixel c 4 2 c3
  
    let ppm = Canvas.toPpm c |> lines

    ppm.[3] |> should equal "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
    ppm.[4] |> should equal "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
    ppm.[5] |> should equal "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"

[<Fact>]
let ``Splitting long lines in PPM files``() =
    let color = Color.make 1.0 0.8 0.6
    let width = 10
    let height = 2
    let pixels = Array2D.create width height color
    let (canvas : Canvas.T) = { Width = width ; Height = height ; Pixels = pixels }

    let ppm = Canvas.toPpm canvas |> lines

    ppm.[3] |> should equal "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
    ppm.[4] |> should equal "153 255 204 153 255 204 153 255 204 153 255 204 153"
    ppm.[5] |> should equal "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
    ppm.[6] |> should equal "153 255 204 153 255 204 153 255 204 153 255 204 153"

[<Fact>]
let ``PPM files are terminated by a newline character``() =
    let ppm = Canvas.make 5 3 |> Canvas.toPpm
    ppm |> should endWith "\n"    