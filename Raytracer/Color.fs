namespace Raytracer

open Constants

type Color =
    { Red: float
      Green: float
      Blue: float }

    static member (+)(c1, c2) =
        { Red = c1.Red + c2.Red
          Green = c1.Green + c2.Green
          Blue = c1.Blue + c2.Blue }

    static member (-)(c1, c2) =
        { Red = c1.Red - c2.Red
          Green = c1.Green - c2.Green
          Blue = c1.Blue - c2.Blue }

    static member (*)(c, scalar) =
        { Red = c.Red * scalar
          Green = c.Green * scalar
          Blue = c.Blue * scalar }

    static member (*)(c1, c2) =
        { Red = c1.Red * c2.Red
          Green = c1.Green * c2.Green
          Blue = c1.Blue * c2.Blue }

    static member (.=)(c1, c2) =
        abs (c1.Red - c2.Red) < Epsilon
        && abs (c1.Green - c2.Green) < Epsilon
        && abs (c1.Blue - c2.Blue) < Epsilon

module Color =
    let make r g b = { Red = r; Green = g; Blue = b }

    let black = make 0. 0. 0.
    let white = make 1. 1. 1.
    let red = make 1. 0. 0.
