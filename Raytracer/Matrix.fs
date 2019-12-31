namespace Raytracer

open Constants

module rec Matrix =
    type T =
        { Dimension : int
          Entries : float [,] }

        static member (.=) (m1, m2 : T) =
            let allWithinEpsilon =
                let len = m1.Dimension - 1
                seq {
                    for r in 0 .. len do
                        for c in 0 .. len do
                            if abs (m1.[r, c] - m2.[r, c]) > Epsilon then
                                yield false
                }
                |> Seq.forall id

            m1.Dimension = m2.Dimension && allWithinEpsilon

        static member (*) (m1, m2) =
            let len = m1.Dimension - 1
            let result = Array2D.zeroCreate m1.Dimension m1.Dimension

            for r in 0 .. len do
                for c in 0 .. len do
                    let row = m1.Entries.[r, *]
                    let col = m2.Entries.[*, c]
                    Array.fold2 (fun sum r c -> sum + r * c) 0.0 row col
                    |> Array2D.set result r c

            { Dimension = m1.Dimension
              Entries = result }
              
        static member (*) (m, p : Point.T) : Point.T =
            let result = multiplyHelper m p
            Point.make result.[0] result.[1] result.[2]
            
        static member (*) (m, v : Vector.T) : Vector.T =
            let result = multiplyHelper m v
            Vector.make result.[0] result.[1] result.[2]

        member x.Item
            with get (r, c) = x.Entries.[r, c]

    let private multiplyHelper m (tuple : obj) : float [] =
        let tArr =
            match tuple with
            | :? Point.T as p -> [| p.X; p.Y; p.Z; p.W |]
            | :? Vector.T as v -> [| v.X; v.Y; v.Z; v.W |]
            | _ -> failwith "Type needs to be point or vector"

        let len = m.Dimension - 1

        seq {
            for r in 0 .. len ->
                let row = m.Entries.[r, *]
                Array.fold2 (fun sum r c -> sum + r * c) 0.0 row tArr
        }
        |> Seq.toArray

    let make rows =
        let dim = List.length rows

        if dim >= 2 && dim <= 4
           && List.forall (fun l -> List.length l = dim) rows then
            { Dimension = dim
              Entries = array2D rows }
        else
            failwith "Matrix must be square with dimension 2, 3 or 4"

    let identity =
        make
            [ [ 1.; 0.; 0.; 0. ]
              [ 0.; 1.; 0.; 0. ]
              [ 0.; 0.; 1.; 0. ]
              [ 0.; 0.; 0.; 1. ] ]

    let transpose m =
        [ for c in [ 0 .. m.Dimension - 1 ] do
            yield m.Entries.[*, c] |> List.ofArray ]
        |> make

    let submatrix m row col =
        let dim = m.Dimension - 1
        let entries = Array2D.zeroCreate dim dim

        for r = 0 to dim do
            if r <> row then
                let r' =
                    if r < row then r else r - 1
                for c = 0 to dim do
                    if c <> col then
                        let c' =
                            if c < col then c else c - 1
                        Array2D.set entries r' c' m.Entries.[r, c]
        { Dimension = dim
          Entries = entries }

    let rec determinant m =
        let entries = m.Entries
        if m.Dimension = 2 then
            entries.[0, 0] * entries.[1, 1] - entries.[0, 1] * entries.[1, 0]
        else
            Array.mapi (fun i entry -> entry * cofactor m 0 i) entries.[0, *]
            |> Array.sum

    and cofactor m row col =
        let m = minor m row col
        if (row + col) % 2 = 0 then m else -m

    and minor m row col = determinant (submatrix m row col)

    let isInvertible m = abs (determinant m) > Epsilon

    let inverse m =
        let d = determinant m
        if abs d < Epsilon then
            failwith "Matrix is not invertible"
        else
            let result = Array2D.zeroCreate m.Dimension m.Dimension
            let len = m.Dimension - 1
            let value row col = (cofactor m row col) / d

            for r in 0 .. len do
                for c in 0 .. len do
                    // Swapping r and c to transpose the matrix
                    Array2D.set result c r (value r c)

            { Dimension = m.Dimension
              Entries = result }
