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
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. 0. -1.0
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 0. -10.0) (Color.make 1. 1. 1.)
    let result = Material.lighting m light position eyev normalv
    result |> should equal (Color.make 1.9 1.9 1.9)

[<Fact>]
let ``Lighting with the eye between light and surface, eye offset 45°``() =
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. (sqrt 2. / 2.) (sqrt 2. / 2.)
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 0. -10.0) (Color.make 1. 1. 1.)
    let result = Material.lighting m light position eyev normalv
    result |> should equal (Color.make 1.0 1.0 1.0)

[<Fact>]
let ``Lighting with eye opposite surface, light offset 45°``() =
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. 0. -1.0
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 10. -10.0) (Color.make 1. 1. 1.)
    let result = Material.lighting m light position eyev normalv
    result .= (Color.make 0.7364 0.7364 0.7364) |> should equal true

[<Fact>]
let ``Lighting with eye in the path of the reflection vector``() =
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. -(sqrt 2. / 2.) -(sqrt 2. / 2.)
    let normalv = Vector.make 0. 0. -1.0
    let light =
        Light.make (Point.make 0. 10. -10.0) (Color.make 1. 1. 1.)
    let result = Material.lighting m light position eyev normalv
    result .= (Color.make 1.6364 1.6364 1.6364) |> should equal true

[<Fact>]
let ``Lighting with the light behind the surface``() =
    let m = Material.make
    let position = Point.origin
    let eyev = Vector.make 0. 0. -1.0
    let normalv = Vector.make 0. 0. -1.0
    let light = Light.make (Point.make 0. 0. 10.) (Color.make 1. 1. 1.)
    let result = Material.lighting m light position eyev normalv
    result |> should equal (Color.make 0.1 0.1 0.1)
