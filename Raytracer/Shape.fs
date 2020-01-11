namespace Raytracer

type ShapeType = Sphere

type Shape =
    { Object : ShapeType
      Intersect : Shape -> Ray -> Shape Intersection list
      Transformation : Matrix }
    member x.InverseTransformation = Matrix.inverse x.Transformation

module Shape =
    let make shape intersectFn =
        { Object = shape
          Intersect = intersectFn
          Transformation = Matrix.identity }

    let intersect (shape : Shape) ray =
        let ray2 = Ray.transform ray shape.InverseTransformation
        shape.Intersect shape ray2

    let transform shape transformation =
        { shape with Transformation = transformation }
