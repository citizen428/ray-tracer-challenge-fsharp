module ColorTests

open FsUnit.Xunit
open Xunit
open Raytracer.Types

[<Fact>]
let ``it can access individual color components``() =
    let color = Color.make -0.5 0.4 1.7
    color.Red |> should equal -0.5
    color.Green |> should equal 0.4
    color.Blue |> should equal 1.7

[<Fact>]
let ``two colors can be added``() =
    let c1 = Color.make 0.9 0.6 0.75
    let c2 = Color.make 0.7 0.1 0.25
    let expected = Color.make 1.6 0.7 1.0
    c1 + c2 |> should equal expected

[<Fact>]
let ``two colors can be subtracted``() =
    let c1 = Color.make 0.7 0.6 0.75
    let c2 = Color.make 0.7 0.1 0.25
    let expected = Color.make 0.0 0.5 0.5
    c1 - c2 |> should equal expected

[<Fact>]
let ``a color can be multiplied by a scalar``() =
    let c = Color.make 0.2 0.3 0.4
    let expected = Color.make 0.4 0.6 0.8
    c * 2.0 |> should equal expected

[<Fact>]
let ``two colors can be multiplied``() =
    let c1 = Color.make 1.0 0.2 0.4
    let c2 = Color.make 0.9 1.0 1.0
    let expected = Color.make 0.9 0.2 0.4
    c1 * c2 |> should equal expected
