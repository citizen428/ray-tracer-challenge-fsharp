module TransformationTests

open FsUnit.Xunit
open Xunit
open Raytracer
open Raytracer.Transformation

[<Fact>]
let ``Multiplying by a translation matrix``() =
    let transform = translation 5. -3.0 2.
    let p = Point.make -3.0 4. 5.
    let expected = Point.make 2. 1. 7.

    transform * p |> should equal expected

[<Fact>]
let ``Multiplying by the inverse translation matrix``() =
    let transform = translation 5. -3.0 2. |> Matrix.inverse
    let p = Point.make -3.0 4. 5.
    let expected = Point.make -8.0 7. 3.

    transform * p |> should equal expected

[<Fact>]
let ``Translation does not affect vectors``() =
    let transform = translation 5. -3.0 2.
    let v = Vector.make -3.0 4. 5.

    transform * v |> should equal v

[<Fact>]
let ``A scaling matrix applied to a point``() =
    let transform = scaling 2. 3. 4.
    let p = Point.make -4.0 6. 8.
    let expected = Point.make -8.0 18. 32.

    transform * p |> should equal expected


[<Fact>]
let ``A scaling matrix applied to a vector``() =
    let transform = scaling 2. 3. 4.
    let v = Vector.make -4.0 6. 8.
    let expected = Vector.make -8.0 18. 32.

    transform * v |> should equal expected

[<Fact>]
let ``Multiplying by the inverse of a scaling matrix``() =
    let transform = scaling 2. 3. 4. |> Matrix.inverse
    let v = Vector.make -4.0 6. 8.
    let expected = Vector.make -2.0 2. 2.

    transform * v |> should equal expected

[<Fact>]
let ``Reflection is scaling by a negative value``() =
    let transform = scaling -1.0 1. 1.
    let p = Point.make 2. 3. 4.
    let expected = Point.make -2.0 3. 4.

    transform * p |> should equal expected

[<Fact>]
let ``Rotating a point around the x axis``() =
    let p = Point.make 0. 1. 0.
    let halfQuarter = rotationX Deg45
    let fullQuarter = rotationX Deg90
    halfQuarter * p .= Point.make 0. (sqrt 2. / 2.) (sqrt 2. / 2.)
    |> should equal true
    fullQuarter * p .= Point.make 0. 0. 1. |> should equal true

[<Fact>]
let ``The inverse of an x rotation rotates in the opposite direction``() =
    let p = Point.make 0. 1. 0.
    let halfQuarter = rotationX Deg45
    let inv = Matrix.inverse halfQuarter
    inv * p .= Point.make 0. (sqrt 2. / 2.) (-sqrt 2. / 2.)
    |> should equal true

[<Fact>]
let ``Rotating a point around the y axis``() =
    let p = Point.make 0. 0. 1.
    let halfQuarter = rotationY Deg45
    let fullQuarter = rotationY Deg90
    halfQuarter * p .= Point.make (sqrt 2. / 2.) 0. (sqrt 2. / 2.)
    |> should equal true
    fullQuarter * p .= Point.make 1. 0. 0. |> should equal true

[<Fact>]
let ``Rotating a point around the z axis``() =
    let p = Point.make 0. 1. 0.
    let halfQuarter = rotationZ Deg45
    let fullQuarter = rotationZ Deg90
    halfQuarter * p .= Point.make (-sqrt 2. / 2.) (sqrt 2. / 2.) 0.
    |> should equal true
    fullQuarter * p .= Point.make -1.0 0. 0. |> should equal true

[<Fact>]
let ``A shearing transformation moves x in proportion to y``() =
    let transform = shearing 1. 0. 0. 0. 0. 0.
    let p = Point.make 2. 3. 4.
    let expected = Point.make 5. 3. 4.
    transform * p |> should equal expected

[<Fact>]
let ``A shearing transformation moves x in proportion to z``() =
    let transform = shearing 0. 1. 0. 0. 0. 0.
    let p = Point.make 2. 3. 4.
    let expected = Point.make 6. 3. 4.
    transform * p |> should equal expected

[<Fact>]
let ``A shearing transformation moves y in proportion to x``() =
    let transform = shearing 0. 0. 1. 0. 0. 0.
    let p = Point.make 2. 3. 4.
    let expected = Point.make 2. 5. 4.
    transform * p |> should equal expected

[<Fact>]
let ``A shearing transformation moves y in proportion to z``() =
    let transform = shearing 0. 0. 0. 1. 0. 0.
    let p = Point.make 2. 3. 4.
    let expected = Point.make 2. 7. 4.
    transform * p |> should equal expected

[<Fact>]
let ``A shearing transformation moves z in proportion to x``() =
    let transform = shearing 0. 0. 0. 0. 1. 0.
    let p = Point.make 2. 3. 4.
    let expected = Point.make 2. 3. 6.
    transform * p |> should equal expected

[<Fact>]
let ``A shearing transformation moves z in proportion to y``() =
    let transform = shearing 0. 0. 0. 0. 0. 1.
    let p = Point.make 2. 3. 4.
    let expected = Point.make 2. 3. 7.
    transform * p |> should equal expected

[<Fact>]
let ``Individual transformation are applied in sequence``() =
    let p = Point.make 1. 0. 1.
    let rotate = rotationX Deg90
    let scale = scaling 5. 5. 5.
    let translate = translation 10. 5. 7.
    // apply rotation first
    let p2 = rotate * p
    p2 .= Point.make 1. -1.0 0. |> should equal true
    // then apply scaling
    let p3 = scale * p2
    p3 .= Point.make 5. -5.0 0. |> should equal true
    // finally apply translation
    let p4 = translate * p3
    p4 .= Point.make 15. 0. 7. |> should equal true

[<Fact>]
let ``Chained transformations must be applied in reverse order``() =
    let p = Point.make 1. 0. 1.
    let rotate = rotationX Deg90
    let scale = scaling 5. 5. 5.
    let translate = translation 10. 5. 7.
    let result = translate * scale * rotate * p
    result .= Point.make 15. 0. 7. |> should equal true

[<Fact>]
let ``The transformation matrix for the default orientation``() =
    let from = Point.origin
    let dest = Point.make 0. 0. -1.
    let up = Vector.make 0. 1. 0.
    let t = Transformation.viewTransform from dest up
    t .= Matrix.identity |> should be True

[<Fact>]
let ``A view transformation matrix looking in positive z direction``() =
    let from = Point.origin
    let dest = Point.make 0. 0. 1.
    let up = Vector.make 0. 1. 0.
    let t = Transformation.viewTransform from dest up
    t .= Transformation.scaling -1. 1. -1. |> should be True

[<Fact>]
let ``The view transformation moves the world, not the eye``() =
    let from = Point.make 0. 0. 8.
    let dest = Point.origin
    let up = Vector.make 0. 1. 0.
    let t = Transformation.viewTransform from dest up
    t |> should equal (Transformation.translation 0. 0. -8.)

[<Fact>]
let ``An arbitrary view transformation``() =
    let from = Point.make 1. 3. 2.
    let dest = Point.make 4. -2. 8.
    let up = Vector.make 1. 1. 0.
    let t = Transformation.viewTransform from dest up
    let expected =
        Matrix.make
            [ [ -0.50709; 0.50709; 0.67612; -2.36643 ]
              [ 0.76772; 0.60609; 0.12122; -2.82843 ]
              [ -0.35857; 0.59761; -0.71714; 0. ]
              [ 0.; 0.; 0.; 1.] ]
    t .= expected |> should be True
