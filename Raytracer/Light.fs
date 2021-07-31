namespace Raytracer

type Light = { Position: Point; Color: Color }

module Light =
    let make position color = { Position = position; Color = color }
