module RayTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``Creating and querying a ray``() =
    let origin = Point.make 1. 2. 3.
    let direction = Vector.make 4. 5. 6.
    let r = Ray.make origin direction
    r.Origin |> should equal origin
    r.Direction |> should equal direction

[<Fact>]
let ``Computing a point from a distance``() =
    let r = Ray.make (Point.make 2. 3. 4.) (Vector.make 1. 0. 0.)
    Ray.position r 0. |> should equal (Point.make 2. 3. 4.)
    Ray.position r 1. |> should equal (Point.make 3. 3. 4.)
    Ray.position r -1.0 |> should equal (Point.make 1. 3. 4.)
    Ray.position r 2.5 |> should equal (Point.make 4.5 3. 4.)

[<Fact>]
let ``Translating a ray``() =
    let r = Ray.make (Point.make 1. 2. 3.) (Vector.make 0. 1. 0.)
    let m = Transformation.translation 3. 4. 5.
    let r2 = Ray.transform r m
    r2.Origin |> should equal (Point.make 4. 6. 8.)
    r2.Direction |> should equal (Vector.make 0. 1. 0.)

[<Fact>]
let ``Scaling a ray``() =
    let r = Ray.make (Point.make 1. 2. 3.) (Vector.make 0. 1. 0.)
    let m = Transformation.scaling 2. 3. 4.
    let r2 = Ray.transform r m
    r2.Origin |> should equal (Point.make 2. 6. 12.)
    r2.Direction |> should equal (Vector.make 0. 3. 0.)
