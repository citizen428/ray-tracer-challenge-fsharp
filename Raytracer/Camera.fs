namespace Raytracer

open System

type Camera =
    { HSize: int
      VSize: int
      FieldOfView: float
      Transform: Matrix
      PixelSize: float
      HalfWidth: float
      HalfHeight: float }

module Camera =
    let makeWithTransform hsize vsize fov transform =
        let halfView = Math.Tan(fov / 2.)
        let aspect = float hsize / float vsize

        let (halfWidth, halfHeight) =
            if aspect >= 1. then
                (halfView, halfView / aspect)
            else
                (halfView * aspect, halfView)

        let pixelSize = halfWidth * 2. / float hsize

        { HSize = hsize
          VSize = vsize
          FieldOfView = fov
          Transform = transform
          PixelSize = pixelSize
          HalfWidth = halfWidth
          HalfHeight = halfHeight }

    let make hsize vsize fov =
        makeWithTransform hsize vsize fov Matrix.identity

    let rayForPixel c px py =
        let xOffset = (float px + 0.5) * c.PixelSize
        let yOffset = (float py + 0.5) * c.PixelSize
        let worldX = c.HalfWidth - xOffset
        let worldY = c.HalfHeight - yOffset
        let inverseTransform = Matrix.inverse c.Transform

        let pixel =
            inverseTransform * (Point.make worldX worldY -1.0)

        let origin = inverseTransform * (Point.make 0. 0. 0.)
        let direction = Vector.normalize (pixel - origin)
        Ray.make origin direction

    let render c w =
        let canvas = Canvas.make c.HSize c.VSize

        for y in 0 .. c.VSize - 1 do
            for x in 0 .. c.HSize - 1 do
                let ray = rayForPixel c x y
                let color = World.colorAt w ray None
                Canvas.writePixel canvas x y color

        canvas
