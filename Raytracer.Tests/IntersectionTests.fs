module IntersectionTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``An intersection encapsulates T and Object``() =
    let s = Sphere.make
    let i = Intersection.make 3.5 s
    i.TValue |> should equal 3.5
    i.Object |> should equal s

[<Fact>]
let ``Aggregating intersections``() =
    let s = Sphere.make
    let i1 = Intersection.make 1. s
    let i2 = Intersection.make 2. s
    let is = Intersection.listOf [ i2; i1 ]
    is.Length |> should equal 2
    is.[0].TValue |> should equal 1.
    is.[1].TValue |> should equal 2.

[<Fact>]
let ``The hit, when all intersections have positive TValues``() =
    let s = Sphere.make
    let i1 = Intersection.make 1. s
    let i2 = Intersection.make 2. s
    let is = Intersection.listOf [ i2; i1 ]
    Intersection.hits is |> should equal (Some i1)

[<Fact>]
let ``The hit, when some intersections have negative TValues``() =
    let s = Sphere.make
    let i1 = Intersection.make -1.0 s
    let i2 = Intersection.make 2. s
    let is = Intersection.listOf [ i2; i1 ]
    Intersection.hits is |> should equal (Some i2)

[<Fact>]
let ``The hit, when all intersections have negative TValues``() =
    let s = Sphere.make
    let i1 = Intersection.make -1.0 s
    let i2 = Intersection.make -2.0 s
    let is = Intersection.listOf [ i2; i1 ]
    Intersection.hits is |> should equal None

[<Fact>]
let ``The hit is always the lowest non-negative intersection``() =
    let s = Sphere.make
    let i1 = Intersection.make 5. s
    let i2 = Intersection.make 7. s
    let i3 = Intersection.make -3.0 s
    let i4 = Intersection.make 2. s
    let is = Intersection.listOf [ i1; i2; i3; i4 ]
    Intersection.hits is |> should equal (Some i4)
