namespace Raytracer

type Pattern =
    { PatternAtFn: Pattern -> Point -> Color
      Transformation: Matrix }
    member x.InverseTransformation = Matrix.inverse x.Transformation

module Pattern =
    let make patternAtFn =
        { PatternAtFn = patternAtFn
          Transformation = Matrix.identity }

    let transform p t = { p with Transformation = t }

    let atShape (p: Pattern) (objTransformation: Matrix) worldPoint =
        let objectPoint =
            Matrix.inverse objTransformation * worldPoint

        let patternPoint = p.InverseTransformation * objectPoint
        p.PatternAtFn p patternPoint

    let stripe (color1: Color) (color2: Color) =
        let patternAt p point =
            let x = floor point.X
            if int x % 2 = 0 then color1 else color2

        make patternAt

    let gradient (color1: Color) (color2: Color) =
        let patternAt p point =
            let colorDistance = color2 - color1
            let fraction = point.X - floor point.X
            color1 + colorDistance * fraction

        make patternAt

    let ring (color1: Color) (color2: Color) =
        let patternAt p point =
            let distance =
                floor (sqrt (point.X ** 2. + point.Z ** 2.))

            let disc = int distance % 2
            if disc = 0 then color1 else color2

        make patternAt

    let checkers (color1: Color) (color2: Color) =
        let patternAt p point =
            let distance =
                floor point.X + floor point.Y + floor point.Z

            let disc = int distance % 2
            if disc = 0 then color1 else color2

        make patternAt
