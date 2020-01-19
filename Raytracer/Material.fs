namespace Raytracer

type Material =
    { Color : Color
      Ambient : float
      Diffuse : float
      Specular : float
      Shininess : float }

module Material =
    let make =
        { Color = Color.white
          Ambient = 0.1
          Diffuse = 0.9
          Specular = 0.9
          Shininess = 200. }

    let withColor c (m : Material) = { m with Color = c }
    let withAmbient a (m : Material) = { m with Ambient = a }
    let withDiffuse d (m : Material) = { m with Diffuse = d }
    let withSpecular s (m : Material) = { m with Specular = s }
    let withShinyness s (m : Material) = { m with Shininess = s }

    let lighting material (light : Light) point eyev normalv =
        // combine the surface color with the light's color/intensity​
        let effectiveColor = material.Color * light.Color
        // find the direction to the light source​
        let lightv = Vector.normalize (light.Position - point)
        // compute the ambient contribution​
        let ambient = effectiveColor * material.Ambient
        // cosine between light and normal vector, negative value represents
        // light on other side of surface
        let lightDotNormal = Vector.dot lightv normalv
        if lightDotNormal < 0. then
            ambient
        else
            // compute the diffuse contribution​
            let diffuse = effectiveColor * material.Diffuse * lightDotNormal
            // cosine between reflection and eye vector, negative value
            // represents reflection away from away
            let reflectv = Vector.reflect -lightv normalv
            let reflectDotEye = Vector.dot reflectv eyev

            if reflectDotEye <= 0. then
                ambient + diffuse
            else
                // compute the specular contribution​
                let factor = reflectDotEye ** material.Shininess
                let specular = light.Color * material.Specular * factor

                ambient + diffuse + specular
