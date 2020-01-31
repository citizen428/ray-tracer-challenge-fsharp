module ComputationTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``Precomputing the state of an intersection``() =
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let s = Sphere.make
    let i = Intersection.make 4. s
    let cs = Computation.prepare i r
    cs.TValue |> should equal i.TValue
    cs.Object |> should equal i.Object
    cs.Point |> should equal (Point.make 0. 0. -1.0)
    cs.EyeV |> should equal (Vector.make 0. 0. -1.0)
    cs.NormalV |> should equal (Vector.make 0. 0. -1.0)

[<Fact>]
let ``The hit, when an intersection occurs on the outside``() =
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let s = Sphere.make
    let i = Intersection.make 4. s
    let cs = Computation.prepare i r
    cs.Inside |> should be False

[<Fact>]
let ``The hit, when an intersection occurs on the inside``() =
    let r = Ray.make (Point.make 0. 0. 0.) (Vector.make 0. 0. 1.)
    let s = Sphere.make
    let i = Intersection.make 1. s
    let cs = Computation.prepare i r
    cs.Point |> should equal (Point.make 0. 0. 1.)
    cs.EyeV |> should equal (Vector.make 0. 0. -1.0)
    cs.Inside |> should be True
    // normal has been inverted
    cs.NormalV |> should equal (Vector.make 0. 0. -1.0)

[<Fact>]
let ``The hit should offset the point``() =
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let t = Transformation.translation 0. 0. 1.
    let s = Shape.transform Sphere.make t
    let i = Intersection.make 5. s
    let cs = Computation.prepare i r
    cs.OverPoint.Z |> should lessThan -(Constants.Epsilon / 2.)
    cs.Point.Z |> should greaterThan cs.OverPoint.Z
