module ShapeTests

open FsUnit.Xunit
open Xunit
open Raytracer

module TestShape =
    let make =
        let intersect (shape : Shape) ray =
            match shape.Object with
            | ShapeType.TestShape data ->
                data.SavedRay <- Some ray
                Intersection.listOf []
            | _ -> failwith "shouldn't ever get here"

        let normalAt (s : Shape) (p : Point) = Vector.make p.X p.Y p.Z
        Shape.make (ShapeType.TestShape { SavedRay = None }) intersect normalAt

[<Fact>]
let ``A shapes's default transformation``() =
    let s = TestShape.make
    s.Transformation |> should equal Matrix.identity

[<Fact>]
let ``Changing a shape's transformation``() =
    let s = TestShape.make
    let t = Transformation.translation 2. 3. 4.
    let s2 = Shape.transform s t
    s2.Transformation |> should equal t

[<Fact>]
let ``A shape has a default material``() =
    let s = TestShape.make
    s.Material |> should equal Material.make

[<Fact>]
let ``A shape can be assinged a material``() =
    let s = TestShape.make
    let m = Material.make |> Material.withAmbient 1.
    let s2 = Shape.texture s m
    s2.Material |> should equal m

[<Fact>]
let ``Intersecting a scaled shape with a ray``() =
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let s =
        Shape.transform TestShape.make (Transformation.scaling 2. 2. 2.)
    let _ = Shape.intersect s r
    match s.Object with
    | TestShape { SavedRay = Some savedRay } ->
        savedRay.Origin |> should equal (Point.make 0. 0. -2.5)
        savedRay.Direction |> should equal (Vector.make 0. 0. 0.5)
    | _ -> failwith "Didn't save ray"

[<Fact>]
let ``Intersecting a translated shape with a ray``() =
    let r = Ray.make (Point.make 0. 0. -5.0) (Vector.make 0. 0. 1.)
    let s =
        Shape.transform TestShape.make (Transformation.translation 5. 0. 0.)
    let _ = Shape.intersect s r
    match s.Object with
    | TestShape { SavedRay = Some savedRay } ->
        savedRay.Origin |> should equal (Point.make -5.0 0. -5.0)
        savedRay.Direction |> should equal (Vector.make 0. 0. 1.)
    | _ -> failwith "Didn't save ray"

[<Fact>]
let ``Computing the normal on a translated shape``() =
    let t = Transformation.translation 0. 1. 0.
    let s = Shape.transform TestShape.make t
    let n = Shape.normalAt s (Point.make 0. 1.70711 -0.70711)
    n .= (Vector.make 0. 0.70711 -0.70711) |> should equal true

[<Fact>]
let ``Computing the normal on a transformed shape``() =
    let scale = Transformation.scaling 1. 0.5 1.
    let rotate = Transformation.rotationZ (System.Math.PI / 5.)
    let t = scale * rotate
    let s = Shape.transform TestShape.make t
    let f = sqrt 2. / 2.
    let n = Shape.normalAt s (Point.make 0. f -f)
    n .= (Vector.make 0. 0.97014 -0.24254) |> should equal true
