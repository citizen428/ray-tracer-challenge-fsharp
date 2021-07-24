#r "../../Raytracer/obj/Debug/net5.0/Raytracer.dll"

open Raytracer
open System.IO

let rayOrigin = Point.make 0. 0. -5.0
let wallZ = 10.
let wallSize = 7.
let canvasSize = 500
let pixelSize = wallSize / (float canvasSize)
let half = wallSize / 2.
let canvas = Canvas.make canvasSize canvasSize

let material =
    Material.make
    |> Material.withColor (Color.make 0.1 0.48 0.76)
    |> Material.withSpecular 0.75

let sphere = Shape.texture Sphere.make material
let lightPosition = Point.make -10.0 10. -10.0
let light = Light.make lightPosition Color.white

for y in 0 .. canvasSize - 1 do
    let worldY = half - pixelSize * float y
    for x in 0 .. canvasSize - 1 do
        let worldX = -half + pixelSize * float x
        let position = Point.make worldX worldY wallZ
        let ray =
            Ray.make rayOrigin (position - rayOrigin |> Vector.normalize)
        let is = Shape.intersect sphere ray

        match Intersection.hits is with
        | None -> ()
        | Some hit ->
            let point = Ray.position ray hit.TValue
            let normal = Shape.normalAt hit.Object point
            let eye = -ray.Direction
            let color =
                Material.lighting hit.Object.Material light point eye normal
                    Exposed

            Canvas.writePixel canvas x y color

File.WriteAllText("img/ch06-sphere-lighting.ppm", Canvas.toPpm canvas)
