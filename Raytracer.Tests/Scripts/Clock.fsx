#load "../../Raytracer/Constants.fs"
#load "../../Raytracer/Vector.fs"
#load "../../Raytracer/Point.fs"
#load "../../Raytracer/Matrix.fs"
#load "../../Raytracer/Transformation.fs"
#load "../../Raytracer/Color.fs"
#load "../../Raytracer/Canvas.fs"

open Raytracer
open System
open System.IO

let origin = Point.make 0. 0. 0.

let drawClock width height =
    let cx = width / 2
    let cy = height / 2

    let twelve = Point.make 0. (float cy * 0.8) 0.
    let hourAngle = Math.PI / 6.

    let canvas = Canvas.make width height

    for hour in 1 .. 12 do
        let tx = Transformation.rotationZ (hourAngle * (float hour))
        let point = tx * twelve
        let x = cx + (int point.X)
        let y = cy - (int point.Y)
        Canvas.writePixel canvas x y Color.white

    canvas

let data = Canvas.toPpm (drawClock 200 200)

File.WriteAllText("img/ch04-clock.ppm", data)
