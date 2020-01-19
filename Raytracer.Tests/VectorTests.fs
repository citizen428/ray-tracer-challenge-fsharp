module VectorTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``it can access the x component``() =
    let vector = Vector.make 4.3 -4.2 3.1
    vector.X |> should equal 4.3

[<Fact>]
let ``it can access the y component``() =
    let vector = Vector.make 4.3 -4.2 3.1
    vector.Y |> should equal -4.2

[<Fact>]
let ``it can access the z component``() =
    let vector = Vector.make 4.3 -4.2 3.1
    vector.Z |> should equal 3.1

[<Fact>]
let ``the w component of vectors is always 0``() =
    let vector = Vector.make 4.3 -4.2 3.1
    vector.W |> should equal 0.0

[<Fact>]
let ``it considers epsilon when testing for equality``() =
    let v1 = Vector.make 0.00001 0. 0.
    let v2 = Vector.make 0. 0. 0.
    v1 .= v2 |> should equal true

[<Fact>]
let ``adding two vectors results in a new vector``() =
    let vector1 = Vector.make 3.0 -2.0 5.0
    let vector2 = Vector.make -2.0 3.0 1.0
    let expected = Vector.make 1.0 1.0 6.0
    vector1 + vector2 .= expected |> should equal true

[<Fact>]
let ``subtracting two vectors results in a new vector``() =
    let vector1 = Vector.make 3.0 2.0 1.0
    let vector2 = Vector.make 5.0 6.0 7.0
    let expected = Vector.make -2.0 -4.0 -6.0
    vector1 - vector2 .= expected |> should equal true

[<Fact>]
let ``negating a vector results in the opposite vector``() =
    let vector = Vector.make 1.0 -2.0 3.0
    let expected = Vector.make -1.0 2.0 -3.0
    -vector .= expected |> should equal true

[<Fact>]
let ``multiplying a vector by a scalar results in a new vector``() =
    let vector = Vector.make 1.0 -2.0 3.0
    let scalar = 2.0
    let expected = Vector.make 2.0 -4.0 6.0
    vector * scalar .= expected |> should equal true

[<Fact>]
let ``dividing a vector by a scalar results in a new vector``() =
    let vector = Vector.make 1.0 -2.0 3.0
    let scalar = 2.0
    let expected = Vector.make 0.5 -1.0 1.5
    vector / scalar .= expected |> should equal true

[<Fact>]
let ``a vector has a magnitude (length)``() =
    let vs =
        [ Vector.make 1.0 0.0 0.0
          Vector.make 0.0 1.0 0.0
          Vector.make 0.0 0.0 1.0
          Vector.make 1.0 2.0 2.0
          Vector.make -1.0 -2.0 -2.0 ]

    let expected = [ 1.0; 1.0; 1.0; 3.0; 3.0 ]
    List.map Vector.magnitude vs |> should equal expected

[<Fact>]
let ``vectors can be normalized to unit vectors``() =
    let vs =
        [ Vector.make 4.0 0.0 0.0
          Vector.make 1.0 2.0 2.0 ]

    let expected =
        [ Vector.make 1.0 0.0 0.0
          Vector.make (1.0 / 3.0) (2.0 / 3.0) (2. / 3.0) ]

    List.map Vector.normalize vs |> should equal expected

[<Fact>]
let ``normalized vectors have a magnitude of 1``() =
    let vector = Vector.make 1.0 -2.0 3.0
    Vector.normalize vector
    |> Vector.magnitude
    |> should equal 1.0

[<Fact>]
let ``the dot product of two vectors is a scalar value``() =
    let vector1 = Vector.make 1.0 2.0 3.0
    let vector2 = Vector.make 2.0 3.0 4.0
    Vector.dot vector1 vector2 |> should equal 20.0

[<Fact>]
let ``the cross product of two vectors is a new vector``() =
    let v1 = Vector.make 1.0 2.0 3.0
    let v2 = Vector.make 2.0 3.0 4.0
    Vector.cross v1 v2 |> should equal (Vector.make -1.0 2.0 -1.0)
    Vector.cross v2 v1 |> should equal (Vector.make 1.0 -2.0 1.0)

[<Fact>]
let ``Reflecting a vector approaching at 45°``() =
    let v = Vector.make 1. -1.0 0.
    let n = Vector.make 0. 1. 0.
    let r = Vector.reflect v n
    r |> should equal (Vector.make 1. 1. 0.)

[<Fact>]
let ``Reflecting a vector off a slanted surface``() =
    let v = Vector.make 0. -1.0 0.
    let n = Vector.make (sqrt 2. / 2.) (sqrt 2. / 2.) 0.
    let r = Vector.reflect v n
    r .= (Vector.make 1. 0. 0.) |> should equal true
