#load "../../Raytracer/Constants.fs"
#load "../../Raytracer/Vector.fs"
#load "../../Raytracer/Point.fs"
#load "../../Raytracer/Color.fs"
#load "../../Raytracer/Canvas.fs"

open System.IO
open Raytracer

type Projectile = { position: Point; velocity: Vector }

type Environment = { gravity: Vector; wind: Vector }

let tick env p =
    { position = p.position + p.velocity
      velocity = p.velocity + env.gravity + env.wind }

let rec fly env p =
    seq {
        if p.position.Y > 0.0 then
            yield p
            yield! p |> tick env |> fly env
    }

let onCanvas (c: Canvas) x y =
    x >= 0 && x < c.Width && y >= 0 && y < c.Height

// gravity -0.1 unit/tick, and wind is -0.01 unit/tick.​
let env =
    { gravity = Vector.make 0.0 -0.1 0.0
      wind = Vector.make -0.01 0.0 0.0 }

// Projectile starts one unit above the origin.​
// Velocity is normalized and multiplied to increase its magnitude.
let projectile =
    { position = Point.make 0.0 1.0 0.0
      velocity =
          (Vector.make 1.0 1.8 0.0 |> Vector.normalize)
          * 11.25 }

let canvas = Canvas.make 900 550
let color = Color.make 1.0 0.7 0.7

for projectile in fly env projectile do
    let canvasX = projectile.position.X |> round |> int

    let canvasY =
        canvas.Height
        - (int <| round projectile.position.Y)

    if onCanvas canvas canvasX canvasY then
        Canvas.writePixel canvas canvasX canvasY color

File.WriteAllText("img/ch02-projectile.ppm", Canvas.toPpm canvas)
