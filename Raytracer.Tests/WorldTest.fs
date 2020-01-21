module WorldTests

open FsUnit.Xunit
open Xunit
open Raytracer

open Material

[<Fact>]
let ``Creating a world``() =
    let w = World.empty
    w.Shapes |> should be Empty
    w.Lights |> should be Empty

[<Fact>]
let ``The default world``() =
    let w = World.defaultWorld
    let l = Light.make (Point.make -10.0 10. -10.0) Color.white

    let m =
        Material.make
        |> withColor (Color.make 0.8 1.0 0.6)
        |> withDiffuse 0.7
        |> withSpecular 0.2

    let t = Transformation.scaling 0.5 0.5 0.5

    w.Lights.[0] |> should equal l
    w.Shapes.[0].Material |> should equal m
    w.Shapes.[1].Transformation |> should equal t

[<Fact>]
let ``Intersect a world with a ray``() =
    let w = World.defaultWorld
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let is = World.intersect w r
    is.Length |> should equal 4
    is.[0].TValue |> should equal 4.
    is.[1].TValue |> should equal 4.5
    is.[2].TValue |> should equal 5.5
    is.[3].TValue |> should equal 6.

[<Fact>]
let ``Shading an intersection``() =
    let w = World.defaultWorld
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let shape = w.Shapes.[0]
    let i = Intersection.make 4. shape
    let comps = Computation.prepare i r
    let c = World.shadeHit w comps
    c .= Color.make 0.38066 0.47583 0.2855 |> should be True

[<Fact>]
let ``Shading an intersection from the inside``() =
    let w =
        World.defaultWorld
        |> World.withLights
            [ Light.make (Point.make 0. 0.25 0.) Color.white ]
    let r = Ray.make (Point.make 0. 0. 0.) (Vector.make 0. 0. 1.)
    let shape = w.Shapes.[1]
    let i = Intersection.make 0.5 shape
    let comps = Computation.prepare i r
    let c = World.shadeHit w comps
    c .= Color.make 0.90498 0.90498 0.90498 |> should be True
