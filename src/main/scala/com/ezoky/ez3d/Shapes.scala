/*
 * @author gweinbach on 24/06/2022 22:42
 * @since 0.2.0
 */

package com.ezoky.ez3d

import com.ezoky.eznumber.Precision

import spire.*
import spire.math.*
import spire.implicits.*

/**
 * @since 0.2.0
 * @author gweinbach on 24/06/2022
 */
trait Shapes[T: Numeric: Precision]
  extends Space[T]:
  
  private val _Numeric = summon[Numeric[T]]
  private val _0 = _Numeric.zero
  private val __1 = _Numeric.one
  private val __2 = _Numeric.fromInt(2)

  case class Square(size: T)
    extends Shape:

    val halfSize = size / __2

    override val vertices: Vertices =
      scala.Vector(
        Vertex(SpacePoint(-halfSize, -halfSize, _0), SpacePoint(halfSize, -halfSize, _0)),
        Vertex(SpacePoint(halfSize, -halfSize, _0), SpacePoint(halfSize, halfSize, _0)),
        Vertex(SpacePoint(halfSize, halfSize, _0), SpacePoint(-halfSize, halfSize, _0)),
        Vertex(SpacePoint(-halfSize, halfSize, _0), SpacePoint(-halfSize, -halfSize, _0))
      )

  case class Cube(size: T)
    extends Shape:

    val halfSize = size / __2

    override val vertices: Vertices =
      scala.Vector(
        Vertex(SpacePoint(-halfSize, -halfSize, -halfSize), SpacePoint(halfSize, -halfSize, -halfSize)),
        Vertex(SpacePoint(halfSize, -halfSize, -halfSize), SpacePoint(halfSize, halfSize, -halfSize)),
        Vertex(SpacePoint(halfSize, halfSize, -halfSize), SpacePoint(-halfSize, halfSize, -halfSize)),
        Vertex(SpacePoint(-halfSize, halfSize, -halfSize), SpacePoint(-halfSize, -halfSize, -halfSize)),

        Vertex(SpacePoint(-halfSize, -halfSize, halfSize), SpacePoint(halfSize, -halfSize, halfSize)),
        Vertex(SpacePoint(halfSize, -halfSize, halfSize), SpacePoint(halfSize, halfSize, halfSize)),
        Vertex(SpacePoint(halfSize, halfSize, halfSize), SpacePoint(-halfSize, halfSize, halfSize)),
        Vertex(SpacePoint(-halfSize, halfSize, halfSize), SpacePoint(-halfSize, -halfSize, halfSize)),

        Vertex(SpacePoint(-halfSize, -halfSize, -halfSize), SpacePoint(-halfSize, -halfSize, halfSize)),
        Vertex(SpacePoint(-halfSize, halfSize, -halfSize), SpacePoint(-halfSize, halfSize, halfSize)),
        Vertex(SpacePoint(halfSize, -halfSize, -halfSize), SpacePoint(halfSize, -halfSize, halfSize)),
        Vertex(SpacePoint(halfSize, halfSize, -halfSize), SpacePoint(halfSize, halfSize, halfSize)),
      )  
      
  case class Parallelepiped(width: T,
                            height: T,
                            depth: T)
      extends Shape:

    val halfWidth = width / __2
    val halfHeight = height / __2
    val halfDepth = depth / __2

    override val vertices: Vertices =
      scala.Vector(
        Vertex(SpacePoint(-halfWidth, -halfHeight, -halfDepth), SpacePoint(halfWidth, -halfHeight, -halfDepth)),
        Vertex(SpacePoint(halfWidth, -halfHeight, -halfDepth), SpacePoint(halfWidth, halfHeight, -halfDepth)),
        Vertex(SpacePoint(halfWidth, halfHeight, -halfDepth), SpacePoint(-halfWidth, halfHeight, -halfDepth)),
        Vertex(SpacePoint(-halfWidth, halfHeight, -halfDepth), SpacePoint(-halfWidth, -halfHeight, -halfDepth)),

        Vertex(SpacePoint(-halfWidth, -halfHeight, halfDepth), SpacePoint(halfWidth, -halfHeight, halfDepth)),
        Vertex(SpacePoint(halfWidth, -halfHeight, halfDepth), SpacePoint(halfWidth, halfHeight, halfDepth)),
        Vertex(SpacePoint(halfWidth, halfHeight, halfDepth), SpacePoint(-halfWidth, halfHeight, halfDepth)),
        Vertex(SpacePoint(-halfWidth, halfHeight, halfDepth), SpacePoint(-halfWidth, -halfHeight, halfDepth)),

        Vertex(SpacePoint(-halfWidth, -halfHeight, -halfDepth), SpacePoint(-halfWidth, -halfHeight, halfDepth)),
        Vertex(SpacePoint(-halfWidth, halfHeight, -halfDepth), SpacePoint(-halfWidth, halfHeight, halfDepth)),
        Vertex(SpacePoint(halfWidth, -halfHeight, -halfDepth), SpacePoint(halfWidth, -halfHeight, halfDepth)),
        Vertex(SpacePoint(halfWidth, halfHeight, -halfDepth), SpacePoint(halfWidth, halfHeight, halfDepth)),
      )
      
  case class Cross(width: T,
                   height: T,
                   depth: T)
    extends Shape:

    val halfWidth = width / __2
    val halfHeight = height / __2
    val halfDepth = depth / __2

    override val vertices: Vertices =
      scala.Vector(
        Vertex(SpacePoint(-halfWidth, _0, _0), SpacePoint(halfWidth, _0, _0)),
        Vertex(SpacePoint(_0, -halfHeight, _0), SpacePoint(_0, halfHeight, _0)),
        Vertex(SpacePoint(_0, _0, -halfDepth), SpacePoint(_0, _0, halfDepth))
      )
      
  object Cross:
    
    def Uniform(size: T): Cross =
      Cross(size, size, size)
      
    lazy val Normal: Cross =
      Uniform(__1)