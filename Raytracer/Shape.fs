namespace Raytracer

module Shape =
    type Type = Sphere

    type IntersectFun = T -> Ray.T -> T Intersection.IntersectionList

    and T =
        { Shape : Type
          Transformation : Matrix.T
          InverseTransformation : Matrix.T
          Intersect : IntersectFun }

    let make (shape : Type) (intersect : IntersectFun) =
        { Shape = shape
          Transformation = Matrix.identity
          InverseTransformation = Matrix.identity
          Intersect = intersect }

    let intersect shape ray =
        let ray2 = Ray.transform ray shape.InverseTransformation
        shape.Intersect shape ray2

    let transform shape transformation =
        { shape with
              Transformation = transformation
              InverseTransformation = Matrix.inverse transformation }
