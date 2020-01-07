module SphereTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``A ray intersects a sphere at two points``() =
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 2
    xs.[0].TValue |> should equal 4.
    xs.[1].TValue |> should equal 6.

[<Fact>]
let ``A ray intersects a sphere at a tangent``() =
    let r = Ray.make (Point.make 0. 1. -5.0) (Vector.make 0. 0. 1.)
    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 2
    xs.[0].TValue |> should equal 5.
    xs.[1].TValue |> should equal 5.

[<Fact>]
let ``A ray misses a sphere``() =
    let r = Ray.make (Point.make 0. 2. -5.0) (Vector.make 0. 0. 1.)
    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 0

[<Fact>]
let ``A ray originates inside a sphere``() =
    let r = Ray.make (Point.make 0. 0. 0.) (Vector.make 0. 0. 1.)
    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 2
    xs.[0].TValue |> should equal -1.0
    xs.[1].TValue |> should equal 1.

[<Fact>]
let ``A sphere is behind a ray``() =
    let r = Ray.make (Point.make 0. 0. 5.) (Vector.make 0. 0. 1.)
    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 2
    xs.[0].TValue |> should equal -6.0
    xs.[1].TValue |> should equal -4.0

[<Fact>]
let ``A sphere's default transformation``() =
    let s = Sphere.make
    s.Transformation |> should equal Matrix.identity

[<Fact>]
let ``Changing a sphere's transformation``() =
    let s = Sphere.make
    let t = Transformation.translation 2. 3. 4.
    let s2 = Shape.transform s t
    s2.Transformation |> should equal t

[<Fact>]
let ``Intersecting a scaled sphere with a ray``() =
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let s = Shape.transform Sphere.make (Transformation.scaling 2. 2. 2.)
    let is = Shape.intersect s r
    is.Length |> should equal 2
    is.[0].TValue |> should equal 3.
    is.[1].TValue |> should equal 7.

[<Fact>]
let ``Intersecting a translated sphere with a ray``() =
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let s =
        Shape.transform Sphere.make (Transformation.translation 5. 0. 0.)
    let is = Shape.intersect s r
    is.Length |> should equal 0
