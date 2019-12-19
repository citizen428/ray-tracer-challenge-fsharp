module CanvasTests

open FsUnit.Xunit
open Xunit
open Raytracer.Types

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
