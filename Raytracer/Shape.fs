namespace Raytracer

type ShapeType = Sphere

type Shape =
    { Object : ShapeType
      Intersect : Shape -> Ray -> Shape Intersection list
      NormalAt : Shape -> Point -> Vector
      Transformation : Matrix
      Material : Material }
    member x.InverseTransformation = Matrix.inverse x.Transformation

module Shape =
    let make shape intersectFn normalAtFn =
        { Object = shape
          Intersect = intersectFn
          NormalAt = normalAtFn
          Transformation = Matrix.identity
          Material = Material.make }

    let intersect (shape : Shape) ray =
        let ray2 = Ray.transform ray shape.InverseTransformation
        shape.Intersect shape ray2

    let transform shape transformation =
        { shape with Transformation = transformation }

    let toObjectSpace (shape : Shape) worldPoint =
        shape.InverseTransformation * worldPoint

    let toWorldSpace (shape : Shape) objNormal =
        let worldNormal =
            Matrix.transpose shape.InverseTransformation * objNormal
        Vector.normalize { worldNormal with W = 0. }

    let normalAt (shape : Shape) point =
        let objPoint = toObjectSpace shape point
        let objNormal = shape.NormalAt shape objPoint
        toWorldSpace shape objNormal

    let texture shape material = { shape with Material = material }
