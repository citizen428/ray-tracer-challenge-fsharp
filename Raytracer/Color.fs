namespace Raytracer

module Color =
    type T =
        { Red : float
          Green : float
          Blue : float }

    let make r g b =
        { Red = r
          Green = g
          Blue = b }

    type T with
        static member (+) (c1, c2) =
            make (c1.Red + c2.Red) (c1.Green + c2.Green) (c1.Blue + c2.Blue)

        static member (-) (c1, c2) =
            make (c1.Red - c2.Red) (c1.Green - c2.Green) (c1.Blue - c2.Blue)

        static member (*) (c, scalar) =
            make (c.Red * scalar) (c.Green * scalar) (c.Blue * scalar)

        static member (*) (c1, c2) =
            make (c1.Red * c2.Red) (c1.Green * c2.Green) (c1.Blue * c2.Blue)

    let black = make 0. 0. 0.
    let white = make 1. 1. 1.
    let red = make 1. 0. 0.
