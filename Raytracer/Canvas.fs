namespace Raytracer.Types

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
