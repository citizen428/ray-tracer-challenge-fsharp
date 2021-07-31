#load "../../Raytracer/Constants.fs"
#load "../../Raytracer/Vector.fs"
#load "../../Raytracer/Point.fs"

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

// gravity -0.1 unit/tick, and wind is -0.01 unit/tick.​
let env =
    { gravity = Vector.make 0.0 -0.1 0.0
      wind = Vector.make -0.01 0.0 0.0 }

// Projectile starts one unit above the origin.​
// Velocity is normalized to 1 unit/tick.​
let projectile =
    { position = Point.make 0.0 1.0 0.0
      velocity = Vector.make 1.0 1.0 0.0 |> Vector.normalize }

fly env projectile
|> Seq.iteri
    (fun i projectile ->
        printf "Tick %-2d:\nPosition: %A\nVelocity: %A\n\n" i (projectile.position) (projectile.velocity))
