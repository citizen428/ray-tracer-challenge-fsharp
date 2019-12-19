#load "../../Raytracer/Tuples.fs"

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
// Velocity is normalized to 1 unit/tick.​
let mutable projectile =
    { position = Point.make 0.0 1.0 0.0
      velocity = Vector.make 1.0 1.0 0.0 |> Vector.normalize }

// gravity -0.1 unit/tick, and wind is -0.01 unit/tick.​
let env =
    { gravity = Vector.make 0.0 -0.1 0.0
      wind = Vector.make -0.01 0.0 0.0 }

let mutable i = 0

while projectile.position.Y > 0.0 do
    printf "Tick %-2d:\nPosition: %A\nVelocity: %A\n" i (projectile.position)
        (projectile.velocity)
    projectile <- tick env projectile
    i <- i + 1
