namespace Raytracer

type Ray = { Origin: Point; Direction: Vector }

module Ray =
    let make origin direction =
        { Origin = origin
          Direction = direction }

    let position ray t = ray.Origin + ray.Direction * t

    let transform ray (m: Matrix) =
        make (m * ray.Origin) (m * ray.Direction)
