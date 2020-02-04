module MaterialTests

open FsUnit.Xunit
open Xunit
open Raytracer

[<Fact>]
let ``The default material``() =
    let m = Material.make
    m.Color |> should equal (Color.make 1. 1. 1.)
    m.Ambient |> should equal 0.1
    m.Diffuse |> should equal 0.9
    m.Specular |> should equal 0.9
    m.Shininess |> should equal 200.

[<Fact>]
let ``Lighting with the eye between the light and the surface``() =
    let s = Sphere.make
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. 0. -1.0
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 0. -10.0) (Color.make 1. 1. 1.)
    let result =
        Material.lighting m s.Transformation light position eyev normalv
            Exposed
    result |> should equal (Color.make 1.9 1.9 1.9)

[<Fact>]
let ``Lighting with the eye between light and surface, eye offset 45°``() =
    let s = Sphere.make
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. (sqrt 2. / 2.) (sqrt 2. / 2.)
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 0. -10.0) (Color.make 1. 1. 1.)
    let result =
        Material.lighting m s.Transformation light position eyev normalv
            Exposed
    result |> should equal (Color.make 1.0 1.0 1.0)

[<Fact>]
let ``Lighting with eye opposite surface, light offset 45°``() =
    let s = Sphere.make
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. 0. -1.0
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 10. -10.0) (Color.make 1. 1. 1.)
    let result =
        Material.lighting m s.Transformation light position eyev normalv
            Exposed
    result .= (Color.make 0.7364 0.7364 0.7364) |> should equal true

[<Fact>]
let ``Lighting with eye in the path of the reflection vector``() =
    let s = Sphere.make
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. -(sqrt 2. / 2.) -(sqrt 2. / 2.)
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 10. -10.0) (Color.make 1. 1. 1.)
    let result =
        Material.lighting m s.Transformation light position eyev normalv
            Exposed
    result .= (Color.make 1.6364 1.6364 1.6364) |> should equal true

[<Fact>]
let ``Lighting with the light behind the surface``() =
    let s = Sphere.make
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. 0. -1.0
    let normalv = Vector.make 0. 0. -1.0
    let light = Light.make (Point.make 0. 0. 10.) (Color.make 1. 1. 1.)
    let result =
        Material.lighting m s.Transformation light position eyev normalv
            Exposed
    result |> should equal (Color.make 0.1 0.1 0.1)

[<Fact>]
let ``Lighting with the surface in shadow``() =
    let s = Sphere.make
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. 0. -1.0
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 0. -10.0) (Color.make 1. 1. 1.)
    let result =
        Material.lighting m s.Transformation light position eyev normalv
            Shadowed
    result |> should equal (Color.make 0.1 0.1 0.1)

[<Fact>]
let ``Lighting with a pattern applied``() =
    let s = Sphere.make

    let m =
        Material.make
        |> Material.withAmbient 1.
        |> Material.withDiffuse 0.
        |> Material.withSpecular 0.
        |> Material.withPattern (Pattern.stripe Color.white Color.black)

    let eyev = Vector.make 0. 0. -1.0
    let normalv = Vector.make 0. 0. -1.0
    let l = Light.make (Point.make 0. 10. -10.0) (Color.make 1. 1. 1.)

    let c1 =
        Material.lighting m s.Transformation l (Point.make 0.9 0. 0.) eyev
            normalv Exposed
    let c2 =
        Material.lighting m s.Transformation l (Point.make 1.1 0. 0.) eyev
            normalv Exposed
    c1 |> should equal Color.white
    c2 |> should equal Color.black
