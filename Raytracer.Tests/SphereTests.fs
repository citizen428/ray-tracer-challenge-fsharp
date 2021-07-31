module SphereTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``A ray intersects a sphere at two points`` () =
    let r =
        Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)

    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 2
    xs.[0].TValue |> should equal 4.
    xs.[1].TValue |> should equal 6.

[<Fact>]
let ``A ray intersects a sphere at a tangent`` () =
    let r =
        Ray.make (Point.make 0. 1. -5.0) (Vector.make 0. 0. 1.)

    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 2
    xs.[0].TValue |> should equal 5.
    xs.[1].TValue |> should equal 5.

[<Fact>]
let ``A ray misses a sphere`` () =
    let r =
        Ray.make (Point.make 0. 2. -5.0) (Vector.make 0. 0. 1.)

    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 0

[<Fact>]
let ``A ray originates inside a sphere`` () =
    let r =
        Ray.make (Point.make 0. 0. 0.) (Vector.make 0. 0. 1.)

    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 2
    xs.[0].TValue |> should equal -1.0
    xs.[1].TValue |> should equal 1.

[<Fact>]
let ``A sphere is behind a ray`` () =
    let r =
        Ray.make (Point.make 0. 0. 5.) (Vector.make 0. 0. 1.)

    let s = Sphere.make
    let xs = Shape.intersect s r
    xs.Length |> should equal 2
    xs.[0].TValue |> should equal -6.0
    xs.[1].TValue |> should equal -4.0

[<Fact>]
let ``Intersecting a scaled sphere with a ray`` () =
    let r =
        Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)

    let s =
        Shape.transform Sphere.make (Transformation.scaling 2. 2. 2.)

    let is = Shape.intersect s r
    is.Length |> should equal 2
    is.[0].TValue |> should equal 3.
    is.[1].TValue |> should equal 7.

[<Fact>]
let ``Intersecting a translated sphere with a ray`` () =
    let r =
        Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)

    let s =
        Shape.transform Sphere.make (Transformation.translation 5. 0. 0.)

    let is = Shape.intersect s r
    is.Length |> should equal 0

[<Fact>]
let ``The normal on a sphere at a point on the x axis`` () =
    let s = Sphere.make
    let n = Shape.normalAt s (Point.make 1. 0. 0.)
    n |> should equal (Vector.make 1. 0. 0.)

[<Fact>]
let ``The normal on a sphere at a point on the y axis`` () =
    let s = Sphere.make
    let n = Shape.normalAt s (Point.make 0. 1. 0.)
    n |> should equal (Vector.make 0. 1. 0.)

[<Fact>]
let ``The normal on a sphere at a point on the z axis`` () =
    let s = Sphere.make
    let n = Shape.normalAt s (Point.make 0. 0. 1.)
    n |> should equal (Vector.make 0. 0. 1.)

[<Fact>]
let ``The normal on a sphere at a nonaxial point`` () =
    let s = Sphere.make
    let f = sqrt 3. / 3.
    let n = Shape.normalAt s (Point.make f f f)
    n .= (Vector.make f f f) |> should equal true
