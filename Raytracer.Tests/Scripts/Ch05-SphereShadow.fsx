#r "../../Raytracer/obj/Debug/net5.0/Raytracer.dll"

open Raytracer
open System
open System.IO

let rayOrigin = Point.make 0. 0. -5.0
let wallZ = 10.
let wallSize = 7.
let canvasSize = 100
let pixelSize = wallSize / (float canvasSize)
let half = wallSize / 2.
let canvas = Canvas.make canvasSize canvasSize
let bgColor = Color.white
let fgColor = Color.black
let sphere = Sphere.make

for y in 0 .. canvasSize - 1 do
    let worldY = half - pixelSize * float y
    for x in 0 .. canvasSize - 1 do
        let worldX = -half + pixelSize * float x
        let position = Point.make worldX worldY wallZ
        let ray =
            Ray.make rayOrigin (position - rayOrigin |> Vector.normalize)
        let is = Shape.intersect sphere ray

        let color =
            match Intersection.hits is with
            | None -> bgColor
            | Some _ -> fgColor

        Canvas.writePixel canvas x y color

File.WriteAllText("img/ch05-sphere-shadow.ppm", Canvas.toPpm canvas)
