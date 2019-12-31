module MatrixTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``Constructing and inspecting a 4x4 matrix``() =
    let m =
        Matrix.make
            [ [ 1.; 2.; 3.; 4. ]
              [ 5.5; 6.5; 7.5; 8.5 ]
              [ 9.; 10.; 11.; 12. ]
              [ 13.5; 14.5; 15.5; 16.5 ] ]
    m.[0, 0] |> should equal 1.
    m.[0, 3] |> should equal 4.
    m.[1, 0] |> should equal 5.5
    m.[1, 2] |> should equal 7.5
    m.[2, 2] |> should equal 11.0
    m.[3, 0] |> should equal 13.5
    m.[3, 2] |> should equal 15.5

[<Fact>]
let ``Constructing and inspecting a 2x2 matrix``() =
    let m =
        Matrix.make
            [ [ -3.0; 5.0 ]
              [ 1.; -2.0 ] ]
    m.[0, 0] |> should equal -3.0
    m.[0, 1] |> should equal 5.
    m.[1, 0] |> should equal 1.
    m.[1, 1] |> should equal -2.0

[<Fact>]
let ``Constructing and inspecting a 3x3 matrix``() =
    let m =
        Matrix.make
            [ [ -3.0; 5.0; 0.0 ]
              [ 1.; -2.0; -7.0 ]
              [ 0.; 1.; 1. ] ]
    m.[0, 0] |> should equal -3.0
    m.[1, 1] |> should equal -2.0
    m.[2, 2] |> should equal 1.

[<Fact>]
let ``Matrix equality with identical matrices``() =
    let m1 =
        Matrix.make
            [ [ 1.; 2.; 3.; 4.000000001 ]
              [ 5.; 6.; 7.; 8. ]
              [ 9.; 8.; 7.; 6. ]
              [ 5.; 4.; 3.; 2. ] ]

    let m2 =
        Matrix.make
            [ [ 1.; 2.; 3.; 4. ]
              [ 5.; 6.; 7.; 8. ]
              [ 9.; 8.; 7.; 6. ]
              [ 5.; 4.; 3.; 2.000000001 ] ]

    m1 .= m2 |> should equal true

[<Fact>]
let ``Matrix equality with different matrices``() =
    let m1 =
        Matrix.make
            [ [ 1.; 2.; 3.; 4. ]
              [ 5.; 6.; 7.; 8. ]
              [ 9.; 8.; 7.; 6. ]
              [ 5.; 4.; 3.; 2. ] ]

    let m2 =
        Matrix.make
            [ [ 1.; 2.; 3.; 5. ]
              [ 5.; 6.; 7.; 8. ]
              [ 9.; 8.; 7.; 6. ]
              [ 5.; 4.; 3.; 2. ] ]

    m1 .= m2 |> should equal false

[<Fact>]
let ``Multiplying two matrices``() =
    let m1 =
        Matrix.make
            [ [ 1.; 2.; 3.; 4. ]
              [ 5.; 6.; 7.; 8. ]
              [ 9.; 8.; 7.; 6. ]
              [ 5.; 4.; 3.; 2. ] ]

    let m2 =
        Matrix.make
            [ [ -2.0; 1.; 2.; 3. ]
              [ 3.; 2.; 1.; -1.0 ]
              [ 4.; 3.; 6.; 5. ]
              [ 1.; 2.; 7.; 8. ] ]

    let expected =
        Matrix.make
            [ [ 20.; 22.; 50.; 48. ]
              [ 44.; 54.; 114.; 108. ]
              [ 40.; 58.; 110.; 102. ]
              [ 16.; 26.; 46.; 42. ] ]

    m1 * m2 .= expected |> should equal true

[<Fact>]
let ``Multiplying a matrix and a vector``() =
    let m =
        Matrix.make
            [ [ 1.; 2.; 3.; 4. ]
              [ 2.; 4.; 4.; 2. ]
              [ 8.; 6.; 4.; 1. ]
              [ 0.; 0.; 0.; 1. ] ]

    let v = Vector.make 1. 2. 3.

    let expected = Vector.make 14. 22. 32.

    m * v .= expected |> should equal true

[<Fact>]
let ``Multiplying a matrix by the identity matrix``() =
    let m =
        Matrix.make
            [ [ 1.; 2.; 3.; 4. ]
              [ 2.; 4.; 4.; 2. ]
              [ 8.; 6.; 4.; 1. ]
              [ 0.; 0.; 0.; 1. ] ]

    m * Matrix.identity .= m |> should equal true

[<Fact>]
let ``Multiplying the identity matrix by a vector``() =
    let v = Vector.make 1. 2. 3.

    Matrix.identity * v .= v |> should equal true

[<Fact>]
let ``Transposing a matrix``() =
    let m =
        Matrix.make
            [ [ 1.; 2.; 3.; 4. ]
              [ 2.; 4.; 4.; 2. ]
              [ 8.; 6.; 4.; 1. ]
              [ 0.; 0.; 0.; 1. ] ]

    let expected =
        Matrix.make
            [ [ 1.; 2.; 8.; 0. ]
              [ 2.; 4.; 6.; 0. ]
              [ 3.; 4.; 4.; 0. ]
              [ 4.; 2.; 1.; 1. ] ]

    Matrix.transpose m .= expected |> should equal true

[<Fact>]
let ``Transposing the identity matrix``() =
    Matrix.transpose Matrix.identity |> should equal Matrix.identity

[<Fact>]
let ``Calculating the determinant of a 2x2 matrix``() =
    let m =
        Matrix.make
            [ [ 1.; 5. ]
              [ -3.0; 2. ] ]

    Matrix.determinant m |> should equal 17.

[<Fact>]
let ``A submatrix of a 3x3 matrix is a 2x2 matrix``() =
    let m =
        Matrix.make
            [ [ 1.; 5.; 0. ]
              [ -3.0; 2.; 7. ]
              [ 0.; 6.; -3.0 ] ]

    let expected =
        Matrix.make
            [ [ -3.0; 2. ]
              [ 0.; 6. ] ]

    Matrix.submatrix m 0 2 |> should equal expected

[<Fact>]
let ``A submatrix of a 4z4 matrix is a 3x3 matrix``() =
    let m =
        Matrix.make
            [ [ 1.; 5.; 0.; 5. ]
              [ -3.0; 2.; 7.; 2. ]
              [ 0.; 6.; -3.0; 1. ]
              [ 3.; 5.; 0.; 9. ] ]

    let expected =
        Matrix.make
            [ [ 1.; 0.; 5. ]
              [ -3.0; 7.; 2. ]
              [ 3.; 0.; 9. ] ]

    Matrix.submatrix m 2 1 |> should equal expected

[<Fact>]
let ``Calculating a minor of a 3x3 matrix``() =
    let m =
        Matrix.make
            [ [ 3.; 5.; 0. ]
              [ 2.; -1.0; -7.0 ]
              [ 6.; -1.0; 5. ] ]

    let subm = Matrix.submatrix m 1 0

    Matrix.determinant subm |> should equal 25.
    Matrix.minor m 1 0 |> should equal 25.

[<Fact>]
let ``Calculating a cofactor of a 3x3 matrix``() =
    let m =
        Matrix.make
            [ [ 3.; 5.; 0. ]
              [ 2.; -1.0; -7.0 ]
              [ 6.; -1.0; 5. ] ]

    Matrix.minor m 0 0 |> should equal -12.0
    Matrix.cofactor m 0 0 |> should equal -12.0
    Matrix.minor m 1 0 |> should equal 25.
    Matrix.cofactor m 1 0 |> should equal -25.0

[<Fact>]
let ``Calculating the determinant of a 3x3 matrix``() =
    let m =
        Matrix.make
            [ [ 1.; 2.; 6. ]
              [ -5.0; 8.; -4.0 ]
              [ 2.; 6.; 4. ] ]

    Matrix.cofactor m 0 0 |> should equal 56.
    Matrix.cofactor m 0 1 |> should equal 12.
    Matrix.cofactor m 0 2 |> should equal -46.0
    Matrix.determinant m |> should equal -196.0

[<Fact>]
let ``Calculating the determinant of a 4x4 matrix``() =
    let m =
        Matrix.make
            [ [ -2.0; -8.0; 3.; 5. ]
              [ -3.0; 1.; 7.; 3. ]
              [ 1.; 2.; -9.0; 6. ]
              [ -6.0; 7.; 7.; -9.0 ] ]

    Matrix.cofactor m 0 0 |> should equal 690.
    Matrix.cofactor m 0 1 |> should equal 447.
    Matrix.cofactor m 0 2 |> should equal 210.
    Matrix.cofactor m 0 3 |> should equal 51.
    Matrix.determinant m |> should equal -4071.0

[<Fact>]
let ``Testing an invertible matrix for invertability``() =
    let m =
        Matrix.make
            [ [ 6.; 4.; 4.; 4. ]
              [ 5.; 5.; 7.; 6. ]
              [ 4.; -9.0; 3.; -7.0 ]
              [ 9.; 1.; 7.; -6.0 ] ]

    Matrix.determinant m |> should equal -2120.0
    Matrix.isInvertible m |> should equal true

[<Fact>]
let ``Testing an non-invertible matrix for invertability``() =
    let m =
        Matrix.make
            [ [ -4.0; 2.; -2.0; 3. ]
              [ 9.; 6.; 2.; 6. ]
              [ 0.; -5.0; 1.; -5.0 ]
              [ 0.; 0.; 0.; 0. ] ]

    Matrix.determinant m |> should equal 0.
    Matrix.isInvertible m |> should equal false

[<Fact>]
let ``Calculating the inverse of a matrix``() =
    let m =
        Matrix.make
            [ [ -5.0; 2.; 6.; -8.0 ]
              [ 1.; -5.0; 1.; 8. ]
              [ 7.; 7.0; -6.0; -7.0 ]
              [ 1.; -3.0; 7.; 4. ] ]

    let expected =
        Matrix.make
            [ [ 0.21805; 0.45113; 0.24060; -0.04511 ]
              [ -0.80827; -1.45677; -0.44361; 0.52068 ]
              [ -0.07895; -0.22368; -0.05263; 0.19737 ]
              [ -0.52256; -0.81391; -0.30075; 0.30639 ] ]

    let mi = Matrix.inverse m

    Matrix.determinant m |> should equal 532.
    Matrix.cofactor m 2 3 |> should equal -160.0
    mi.[3, 2] |> should equal (-160.0 / 532.)
    Matrix.cofactor m 3 2 |> should equal 105.
    mi.[2, 3] |> should equal (105. / 532.)
    Matrix.inverse m .= expected |> should equal true

[<Fact>]
let ``Calculating the inverse of another matrix``() =
    let m =
        Matrix.make
            [ [ 8.; -5.0; 9.; 2. ]
              [ 7.; 5.; 6.; 1. ]
              [ -6.0; 0.; 9.; 6. ]
              [ -3.0; 0.; -9.0; -4.0 ] ]

    let expected =
        Matrix.make
            [ [ -0.15385; -0.15385; -0.28205; -0.53846 ]
              [ -0.07692; 0.12308; 0.02564; 0.03077 ]
              [ 0.35897; 0.35897; 0.43590; 0.92308 ]
              [ -0.69231; -0.69231; -0.76923; -1.92308 ] ]

    Matrix.inverse m .= expected |> should equal true

[<Fact>]
let ``Calculating the inverse of a third matrix``() =
    let m =
        Matrix.make
            [ [ 9.; 3.; 0.; 9. ]
              [ -5.0; -2.0; -6.0; -3.0 ]
              [ -4.0; 9.; 6.; 4. ]
              [ -7.0; 6.; 6.; 2. ] ]

    let expected =
        Matrix.make
            [ [ -0.04074; -0.07778; 0.14444; -0.22222 ]
              [ -0.07778; 0.03333; 0.36667; -0.33333 ]
              [ -0.02901; -0.1463; -0.10926; 0.12963 ]
              [ 0.17778; 0.06667; -0.26667; 0.33333 ] ]

    Matrix.inverse m .= expected |> should equal true

[<Fact>]
let ``Multiplying a product by its inverse``() =
    let m1 =
        Matrix.make
            [ [ 3.; -9.0; 7.; 3. ]
              [ 3.; -8.0; 2.; -9.0 ]
              [ -4.0; 4.; 4.; 1. ]
              [ -6.0; 5.; -1.0; 1. ] ]

    let m2 =
        Matrix.make
            [ [ 8.; 2.; 2.; 2. ]
              [ 3.; -1.0; 7.; 0. ]
              [ 7.; 0.; 5.; 4. ]
              [ 6.; -2.0; 0.; 5. ] ]

    let product = m1 * m2

    product * Matrix.inverse m2 .= m1 |> should equal true
