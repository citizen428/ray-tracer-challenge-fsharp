namespace Raytracer

open System
open System.Text.RegularExpressions

module Canvas =
    type T =
        { Width : int
          Height : int
          Pixels : Color.T [,] }

    let make width height =
        { Width = width
          Height = height
          Pixels = Array2D.create height width Color.black }

    let writePixel canvas x y color = Array2D.set canvas.Pixels y x color

    let pixelAt canvas x y = Array2D.get canvas.Pixels y x

    let toPpm canvas =
        let clamp f =
            let rgbVal = 255.0 * f |> round
            Math.Clamp(int rgbVal, 0, 255)

        let colorToRgb (c : Color.T) =
            sprintf "%d %d %d" (clamp c.Red) (clamp c.Green) (clamp c.Blue)

        let splitLongLines (rgbs : seq<string>) =
            let row = String.Join(" ", rgbs)
            Regex.Replace(row, "[\s\S]{1,69}(?!\S)",
                          (fun m -> m.Value.TrimStart(' ') + "\n"))
                 .TrimEnd('\n')

        let pixelsToString canvas =
            canvas.Pixels
            |> Array2D.map colorToRgb
            |> Seq.cast<string>
            |> Seq.chunkBySize canvas.Width
            |> Seq.map splitLongLines
            |> String.concat "\n"

        let header =
            sprintf "P3\n%d %d\n255" canvas.Width canvas.Height

        let pixels = pixelsToString canvas
        sprintf "%s\n%s\n" header pixels
