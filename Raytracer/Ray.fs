namespace Raytracer

module Ray =
    type T =
        { Origin : Point.T
          Direction : Vector.T }

    let make origin direction =
        { Origin = origin
          Direction = direction }

    let position ray t = ray.Origin + ray.Direction * t

    let transform ray (m : Matrix.T) =
        make (m * ray.Origin) (m * ray.Direction)
