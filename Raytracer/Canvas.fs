namespace Raytracer.Types

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
          Pixels = Array2D.create width height Color.black }

    let writePixel canvas x y color = Array2D.set canvas.Pixels x y color

    let pixelAt canvas x y = Array2D.get canvas.Pixels x y

    let toPpm canvas =
        let clamp f =
            let rgbVal = 255.0 * f |> round
            Math.Clamp(int rgbVal, 0, 255)

        let colorToRgb (c : Color.T) =
            sprintf "%d %d %d" (clamp c.Red) (clamp c.Green) (clamp c.Blue)

        let generateRows (rgbs : seq<string>) =
            let row = String.Join(" ", rgbs)
            Regex.Replace(row, "[\s\S]{1,69}(?!\S)",
                          (fun m -> m.Value.TrimStart(' ') + "\n"))
                 .TrimEnd('\n')

        let pixelsToString pixels =
            pixels
            |> Array2D.map colorToRgb
            |> Seq.cast<string>
            |> Seq.chunkBySize (Array2D.length1 pixels)
            |> Seq.map generateRows
            |> fun strings -> String.Join("\n", strings)

        let header =
            sprintf "P3\n%d %d\n255" canvas.Width canvas.Height

        let pixels = pixelsToString canvas.Pixels
        sprintf "%s\n%s\n" header pixels
