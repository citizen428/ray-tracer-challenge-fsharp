module LightTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``A point light has a position and intensity``() =
    let position = Point.origin
    let intensity = Color.white
    let light = Light.make position intensity
    light.Position |> should equal position
    light.Color |> should equal intensity
