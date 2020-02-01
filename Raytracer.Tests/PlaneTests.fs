module PlaneTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``The normal of a plane is constant everywhere``() =
    let p = Plane.make
    let expected = Vector.make 0. 1. 0.
    p.NormalAt p (Point.origin) |> should equal expected
    p.NormalAt p (Point.make 10. 0. -10.0) |> should equal expected
    p.NormalAt p (Point.make -5.0 0. 150.) |> should equal expected

[<Fact>]
let ``Intersect with a ray parallel to the plane``() =
    let p = Plane.make
    let r = Ray.make (Point.make 0. 10. 0.) (Vector.make 0. 0. 1.)
    let is = p.Intersect p r
    is |> should be Empty

[<Fact>]
let ``Intersect with a coplanar ray``() =
    let p = Plane.make
    let r = Ray.make (Point.make 0. 0. 0.) (Vector.make 0. 0. 1.)
    let is = p.Intersect p r
    is |> should be Empty

[<Fact>]
let ``A ray intersecting a plane from above``() =
    let p = Plane.make
    let r = Ray.make (Point.make 0. 1. 0.) (Vector.make 0. -1.0 0.)
    let is = p.Intersect p r
    is.Length |> should equal 1
    is.[0].TValue |> should equal 1.
    is.[0].Object |> should equal p

[<Fact>]
let ``A ray intersecting a plane from below``() =
    let p = Plane.make
    let r = Ray.make (Point.make 0. -1.0 0.) (Vector.make 0. 1. 0.)
    let is = p.Intersect p r
    is.Length |> should equal 1
    is.[0].TValue |> should equal 1.
    is.[0].Object |> should equal p
