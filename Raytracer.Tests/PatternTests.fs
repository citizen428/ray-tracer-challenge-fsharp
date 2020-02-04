module PatternTests

open FsUnit.Xunit
open Xunit
open Raytracer

module TestPattern =
    let patternAt p (point : Point) = Color.make point.X point.Y point.Z
    let make = Pattern.make patternAt

[<Fact>]
let ``The default pattern transformation``() =
    let p = TestPattern.make
    p.Transformation .= Matrix.identity |> should be True

[<Fact>]
let ``Assigning a transformation``() =
    let p = TestPattern.make
    let t = Transformation.translation 1. 2. 3.
    let p2 = Pattern.transform p t
    p2.Transformation |> should equal t

[<Fact>]
let ``A pattern with an object transformation``() =
    let s = Shape.transform Sphere.make (Transformation.scaling 2. 2. 2.)
    let p = Point.make 2. 3. 4.
    let c = Pattern.atShape TestPattern.make s.Transformation p
    c |> should equal (Color.make 1. 1.5 2.)

[<Fact>]
let ``A pattern with a pattern transformation``() =
    let s = Sphere.make
    let tp = TestPattern.make
    let pattern = Pattern.transform tp (Transformation.scaling 2. 2. 2.)
    let p = Point.make 2. 3. 4.
    let c = Pattern.atShape pattern s.Transformation p
    c |> should equal (Color.make 1. 1.5 2.)

[<Fact>]
let ``A pattern with both an object and a pattern transformation``() =
    let s = Shape.transform Sphere.make (Transformation.scaling 2. 2. 2.)
    let tp = TestPattern.make
    let pattern =
        Pattern.transform tp (Transformation.translation 0.5 1. 1.5)
    let p = Point.make 2.5 3. 3.5
    let c = Pattern.atShape pattern s.Transformation p
    c |> should equal (Color.make 0.75 0.5 0.25)


[<Fact>]
let ``A stripe pattern is constant in y``() =
    let s = Sphere.make
    let p = Pattern.stripe Color.white Color.black
    Pattern.atShape p s.Transformation (Point.origin)
    |> should equal Color.white
    Pattern.atShape p s.Transformation (Point.make 0. 1. 0.)
    |> should equal Color.white
    Pattern.atShape p s.Transformation (Point.make 0. 2. 0.)
    |> should equal Color.white

[<Fact>]
let ``A stripe pattern is constant in z``() =
    let s = Sphere.make
    let p = Pattern.stripe Color.white Color.black
    Pattern.atShape p s.Transformation (Point.origin)
    |> should equal Color.white
    Pattern.atShape p s.Transformation (Point.make 0. 0. 1.)
    |> should equal Color.white
    Pattern.atShape p s.Transformation (Point.make 0. 0. 2.)
    |> should equal Color.white

[<Fact>]
let ``A stripe pattern alternates in x``() =
    let s = Sphere.make
    let p = Pattern.stripe Color.white Color.black
    Pattern.atShape p s.Transformation (Point.origin)
    |> should equal Color.white
    Pattern.atShape p s.Transformation (Point.make 0.9 0. 0.)
    |> should equal Color.white
    Pattern.atShape p s.Transformation (Point.make 1. 0. 0.)
    |> should equal Color.black
    Pattern.atShape p s.Transformation (Point.make -0.1 0. 0.)
    |> should equal Color.black
    Pattern.atShape p s.Transformation (Point.make -1.0 0. 0.)
    |> should equal Color.black
    Pattern.atShape p s.Transformation (Point.make -1.1 0. 0.)
    |> should equal Color.white

[<Fact>]
let ``Stripes with an s transformation``() =
    let s = Shape.transform Sphere.make (Transformation.scaling 2. 2. 2.)
    let p = Pattern.stripe Color.white Color.black
    let c = Pattern.atShape p s.Transformation (Point.make 1.5 0. 0.)
    c |> should equal Color.white

[<Fact>]
let ``Stripes with a pattern transformation``() =
    let s = Sphere.make
    let t = Transformation.scaling 2. 2. 2.
    let p = Pattern.transform (Pattern.stripe Color.white Color.black) t
    let c = Pattern.atShape p s.Transformation (Point.make 1.5 0. 0.)
    c |> should equal Color.white

[<Fact>]
let ``Stripes with both an s and a pattern transformation``() =
    let s = Shape.transform Sphere.make (Transformation.scaling 2. 2. 2.)
    let t = Transformation.translation 0.5 0. 0.
    let p = Pattern.transform (Pattern.stripe Color.white Color.black) t
    let c = Pattern.atShape p s.Transformation (Point.make 2.5 0. 0.)
    c |> should equal Color.white

[<Fact>]
let ``A gradient linearly interpolates between colors``() =
    let p = Pattern.gradient Color.white Color.black
    p.PatternAtFn p Point.origin |> should equal Color.white
    p.PatternAtFn p (Point.make 0.25 0. 0.)
    |> should equal (Color.make 0.75 0.75 0.75)
    p.PatternAtFn p (Point.make 0.5 0. 0.)
    |> should equal (Color.make 0.5 0.5 0.5)
    p.PatternAtFn p (Point.make 0.75 0. 0.)
    |> should equal (Color.make 0.25 0.25 0.25)

[<Fact>]
let ``A ring should extend in both x and z``() =
    let p = Pattern.ring Color.white Color.black
    p.PatternAtFn p (Point.make 0. 0. 0.) |> should equal Color.white
    p.PatternAtFn p (Point.make 1. 0. 0.) |> should equal Color.black
    p.PatternAtFn p (Point.make 0. 0. 1.) |> should equal Color.black
    p.PatternAtFn p (Point.make 0.708 0. 0.708) |> should equal Color.black

[<Fact>]
let ``Checkers should repeat in x``() =
    let p = Pattern.checkers Color.white Color.black
    p.PatternAtFn p Point.origin |> should equal Color.white
    p.PatternAtFn p (Point.make 0.99 0. 0.) |> should equal Color.white
    p.PatternAtFn p (Point.make 1.01 0. 0.) |> should equal Color.black

[<Fact>]
let ``Checkers should repeat in y``() =
    let p = Pattern.checkers Color.white Color.black
    p.PatternAtFn p Point.origin |> should equal Color.white
    p.PatternAtFn p (Point.make 0. 0.99 0.) |> should equal Color.white
    p.PatternAtFn p (Point.make 0. 1.01 0.) |> should equal Color.black

[<Fact>]
let ``Checkers should repeat in z``() =
    let p = Pattern.checkers Color.white Color.black
    p.PatternAtFn p Point.origin |> should equal Color.white
    p.PatternAtFn p (Point.make 0. 0. 0.99) |> should equal Color.white
    p.PatternAtFn p (Point.make 0. 0. 1.01) |> should equal Color.black
