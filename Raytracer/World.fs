namespace Raytracer

open Material

type World =
    { Shapes: Shape list
      Lights: Light list }

module World =
    let make shapes lights = { Shapes = shapes; Lights = lights }

    let empty = make [] []

    let withShapes shapes world = { world with Shapes = shapes }

    let withLights lights world = { world with Lights = lights }

    let defaultWorld =
        let l =
            Light.make (Point.make -10.0 10. -10.0) Color.white

        let m =
            Material.make
            |> withColor (Color.make 0.8 1.0 0.6)
            |> withDiffuse 0.7
            |> withSpecular 0.2

        let s1 = Shape.texture Sphere.make m

        let s2 =
            Shape.transform Sphere.make (Transformation.scaling 0.5 0.5 0.5)

        { Shapes = [ s1; s2 ]; Lights = [ l ] }

    let intersect w r =
        w.Shapes
        |> List.collect (fun s -> Shape.intersect s r)
        |> Intersection.listOf

    let isShadowed w l p =
        let v = l.Position - p
        let distance = Vector.magnitude v
        let direction = Vector.normalize v

        let r = Ray.make p direction
        let is = intersect w r

        match Intersection.hits is with
        | Some hit when hit.TValue < distance -> Shadowed
        | _ -> Exposed

    let rec shadeHit (w: World) (c: Shape Computation) remaining0 =
        let remaining = remaining0 |> Option.defaultValue 4

        w.Lights
        |> List.fold
            (fun color light ->
                let visibility = isShadowed w light c.OverPoint

                let surface =
                    Material.lighting
                        c.Object.Material
                        c.Object.Transformation
                        light
                        c.OverPoint
                        c.EyeV
                        c.NormalV
                        visibility

                let reflected = reflectedColor w c (Some remaining)
                color + surface + reflected)
            Color.black

    and colorAt w r remaining0 =
        let remaining = remaining0 |> Option.defaultValue 4
        let is = intersect w r

        match Intersection.hits is with
        | None -> Color.black
        | Some hit ->
            let cs = Computation.prepare hit r
            shadeHit w cs (Some remaining)

    and reflectedColor w (c: Shape Computation) remaining0 =
        let remaining = remaining0 |> Option.defaultValue 4

        if remaining < 1 then
            Color.black
        elif c.Object.Material.Reflective = 0. then
            Color.black
        else
            let reflectRay = Ray.make c.OverPoint c.ReflectV

            let color =
                colorAt w reflectRay (Some(remaining - 1))

            color * c.Object.Material.Reflective
