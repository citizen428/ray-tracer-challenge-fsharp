namespace Raytracer

open Material

type World =
    { Shapes : Shape list
      Lights : Light list }

module World =
    let make shapes lights =
        { Shapes = shapes
          Lights = lights }

    let empty = make [] []

    let withShapes shapes world = { world with Shapes = shapes }

    let withLights lights world = { world with Lights = lights }

    let defaultWorld =
        let l = Light.make (Point.make -10.0 10. -10.0) Color.white

        let m =
            Material.make
            |> withColor (Color.make 0.8 1.0 0.6)
            |> withDiffuse 0.7
            |> withSpecular 0.2

        let s1 = Shape.texture Sphere.make m
        let s2 =
            Shape.transform Sphere.make (Transformation.scaling 0.5 0.5 0.5)
        { Shapes = [ s1; s2 ]
          Lights = [ l ] }

    let intersect w r =
        w.Shapes
        |> List.collect (fun s -> Shape.intersect s r)
        |> Intersection.listOf

    let shadeHit (w : World) (c : Shape Computation) =
        w.Lights
        |> List.fold (fun color light ->
            let c =
                Material.lighting c.Object.Material light c.Point c.EyeV
                    c.NormalV Exposed
            color + c) Color.black

    let colorAt w r =
        let is = intersect w r
        match Intersection.hits is with
        | None -> Color.black
        | Some hit ->
            let cs = Computation.prepare hit r
            shadeHit w cs
