#load "../../Raytracer/Color.fs"
#load "../../Raytracer/Canvas.fs"
#load "../../Raytracer/Tuples.fs"

open System.IO
open Raytracer.Types
open Raytracer.Types.Tuples

type Projectile =
    { position : Point.T
      velocity : Vector.T }

type Environment =
    { gravity : Vector.T
      wind : Vector.T }

let tick env p =
    { position = p.position + p.velocity
      velocity = p.velocity + env.gravity + env.wind }

// Projectile starts one unit above the origin.​
// Velocity is normalized and multiplied to increase its magnitude.
let mutable projectile =
    { position = Point.make 0.0 1.0 0.0
      velocity = (Vector.make 1.0 1.8 0.0 |> Vector.normalize) * 11.25 }

// gravity -0.1 unit/tick, and wind is -0.01 unit/tick.​
let env =
    { gravity = Vector.make 0.0 -0.1 0.0
      wind = Vector.make -0.01 0.0 0.0 }

let canvas = Canvas.make 900 550
let color = Color.make 1.0 0.7 0.7

while projectile.position.Y > 0.0 do
    let canvasX =
        projectile.position.X
        |> round
        |> int

    let canvasY = canvas.Height - (int <| round projectile.position.Y)
    if canvasX >= 0 && canvasX < canvas.Width && canvasY >= 0
       && canvasY < canvas.Height then
        Canvas.writePixel canvas canvasX canvasY color
    projectile <- tick env projectile

File.WriteAllText("img/ch02-projectile.ppm", Canvas.toPpm canvas)
