module TuplesTests

open FsUnit.Xunit
open Xunit
open Raytracer.Types.Tuples

module PointTests =
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
    let ``the w component of points is always 1.0``() =
        let point = Point.make 4.3 -4.2 3.1
        point.W |> should equal 1.0

    [<Fact>]
    let ``adding a vector to a point results in a new point``() =
        let point = Point.make 3.0 -2.0 5.0
        let vector = Vector.make -2.0 3.0 1.0
        let expected = Point.make 1.0 1.0 6.0
        point + vector |> should equal expected

    [<Fact>]
    let ``subtracting a vector from a point results in a new point``() =
        let point = Point.make 3.0 2.0 1.0
        let vector = Vector.make 5.0 6.0 7.0
        let expected = Point.make -2.0 -4.0 -6.0
        point - vector |> should equal expected

    [<Fact>]
    let ``subtracting two points gives the vector between them``() =
        let point1 = Point.make 3.0 2.0 1.0
        let point2 = Point.make 5.0 6.0 7.0
        let expected = Vector.make -2.0 -4.0 -6.0
        point1 - point2 |> should equal expected

module VectorTests =
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
    let ``the w component of vectors is always 0.0``() =
        let vector = Vector.make 4.3 -4.2 3.1
        vector.W |> should equal 0.0

    [<Fact>]
    let ``adding two vectors results in a new vector``() =
        let vector1 = Vector.make 3.0 -2.0 5.0
        let vector2 = Vector.make -2.0 3.0 1.0
        let expected = Vector.make 1.0 1.0 6.0
        vector1 + vector2 |> should equal expected

    [<Fact>]
    let ``subtracting two vectors results in a new vector``() =
        let vector1 = Vector.make 3.0 2.0 1.0
        let vector2 = Vector.make 5.0 6.0 7.0
        let expected = Vector.make -2.0 -4.0 -6.0
        vector1 - vector2 |> should equal expected

    [<Fact>]
    let ``negating a vector results in the opposite vector``() =
        let vector = Vector.make 1.0 -2.0 3.0
        let expected = Vector.make -1.0 2.0 -3.0
        -vector |> should equal expected

    [<Fact>]
    let ``multiplying a vector by a scalar results in a new vector``() =
        let vector = Vector.make 1.0 -2.0 3.0
        let scalar = 2.0
        let expected = Vector.make 2.0 -4.0 6.0
        vector * scalar |> should equal expected

    [<Fact>]
    let ``dividing a vector by a scalar results in a new vector``() =
        let vector = Vector.make 1.0 -2.0 3.0
        let scalar = 2.0
        let expected = Vector.make 0.5 -1.0 1.5
        vector / scalar |> should equal expected

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
    let ``normalized vectors have a magnitude of 1.0``() =
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
