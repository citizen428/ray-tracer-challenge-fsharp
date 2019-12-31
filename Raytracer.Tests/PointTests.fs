module PointTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``it can access the x component``() =
    let point = Point.make 4.3 -4.2 3.1
    point.X |> should equal 4.3

[<Fact>]
let ``it can access the y component``() =
    let point = Point.make 4.3 -4.2 3.1
    point.Y |> should equal -4.2

[<Fact>]
let ``it can access the z component``() =
    let point = Point.make 4.3 -4.2 3.1
    point.Z |> should equal 3.1

[<Fact>]
let ``the w component of points is always 1``() =
    let point = Point.make 4.3 -4.2 3.1
    point.W |> should equal 1.0

[<Fact>]
let ``adding a vector to a point results in a new point``() =
    let point = Point.make 3.0 -2.0 5.0
    let vector = Vector.make -2.0 3.0 1.0
    let expected = Point.make 1.0 1.0 6.0
    point + vector .= expected |> should equal true

[<Fact>]
let ``subtracting a vector from a point results in a new point``() =
    let point = Point.make 3.0 2.0 1.0
    let vector = Vector.make 5.0 6.0 7.0
    let expected = Point.make -2.0 -4.0 -6.0
    point - vector .= expected |> should equal true

[<Fact>]
let ``subtracting two points gives the vector between them``() =
    let point1 = Point.make 3.0 2.0 1.0
    let point2 = Point.make 5.0 6.0 7.0
    let expected = Vector.make -2.0 -4.0 -6.0
    point1 - point2 |> should equal expected

[<Fact>]
let ``it considers epsilon when testing for equality``() =
    let p1 = Point.make 0.00001 0. 0.
    let p2 = Point.make 0. 0. 0.
    p1 .= p2 |> should equal true
