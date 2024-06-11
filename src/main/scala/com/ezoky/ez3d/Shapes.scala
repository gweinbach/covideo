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
  private val __0 = _Numeric.zero
  private val __1 = _Numeric.one
  private val __2 = _Numeric.fromInt(2)

  case class Square(size: T)
    extends Shape:

    val halfSize = size / __2

    override val segments: Segments =
      scala.Vector(
        Segment(SpacePoint(-halfSize, -halfSize, __0), SpacePoint(halfSize, -halfSize, __0)),
        Segment(SpacePoint(halfSize, -halfSize, __0), SpacePoint(halfSize, halfSize, __0)),
        Segment(SpacePoint(halfSize, halfSize, __0), SpacePoint(-halfSize, halfSize, __0)),
        Segment(SpacePoint(-halfSize, halfSize, __0), SpacePoint(-halfSize, -halfSize, __0))
      )

  case class Cube(size: T)
    extends Shape:

    val halfSize = size / __2

    override val segments: Segments =
      scala.Vector(
        Segment(SpacePoint(-halfSize, -halfSize, -halfSize), SpacePoint(halfSize, -halfSize, -halfSize)),
        Segment(SpacePoint(halfSize, -halfSize, -halfSize), SpacePoint(halfSize, halfSize, -halfSize)),
        Segment(SpacePoint(halfSize, halfSize, -halfSize), SpacePoint(-halfSize, halfSize, -halfSize)),
        Segment(SpacePoint(-halfSize, halfSize, -halfSize), SpacePoint(-halfSize, -halfSize, -halfSize)),

        Segment(SpacePoint(-halfSize, -halfSize, halfSize), SpacePoint(halfSize, -halfSize, halfSize)),
        Segment(SpacePoint(halfSize, -halfSize, halfSize), SpacePoint(halfSize, halfSize, halfSize)),
        Segment(SpacePoint(halfSize, halfSize, halfSize), SpacePoint(-halfSize, halfSize, halfSize)),
        Segment(SpacePoint(-halfSize, halfSize, halfSize), SpacePoint(-halfSize, -halfSize, halfSize)),

        Segment(SpacePoint(-halfSize, -halfSize, -halfSize), SpacePoint(-halfSize, -halfSize, halfSize)),
        Segment(SpacePoint(-halfSize, halfSize, -halfSize), SpacePoint(-halfSize, halfSize, halfSize)),
        Segment(SpacePoint(halfSize, -halfSize, -halfSize), SpacePoint(halfSize, -halfSize, halfSize)),
        Segment(SpacePoint(halfSize, halfSize, -halfSize), SpacePoint(halfSize, halfSize, halfSize)),
      )  
      
  case class Parallelepiped(width: T,
                            height: T,
                            depth: T)
      extends Shape:

    val halfWidth = width / __2
    val halfHeight = height / __2
    val halfDepth = depth / __2

    override val segments: Segments =
      scala.Vector(
        Segment(SpacePoint(-halfWidth, -halfHeight, -halfDepth), SpacePoint(halfWidth, -halfHeight, -halfDepth)),
        Segment(SpacePoint(halfWidth, -halfHeight, -halfDepth), SpacePoint(halfWidth, halfHeight, -halfDepth)),
        Segment(SpacePoint(halfWidth, halfHeight, -halfDepth), SpacePoint(-halfWidth, halfHeight, -halfDepth)),
        Segment(SpacePoint(-halfWidth, halfHeight, -halfDepth), SpacePoint(-halfWidth, -halfHeight, -halfDepth)),

        Segment(SpacePoint(-halfWidth, -halfHeight, halfDepth), SpacePoint(halfWidth, -halfHeight, halfDepth)),
        Segment(SpacePoint(halfWidth, -halfHeight, halfDepth), SpacePoint(halfWidth, halfHeight, halfDepth)),
        Segment(SpacePoint(halfWidth, halfHeight, halfDepth), SpacePoint(-halfWidth, halfHeight, halfDepth)),
        Segment(SpacePoint(-halfWidth, halfHeight, halfDepth), SpacePoint(-halfWidth, -halfHeight, halfDepth)),

        Segment(SpacePoint(-halfWidth, -halfHeight, -halfDepth), SpacePoint(-halfWidth, -halfHeight, halfDepth)),
        Segment(SpacePoint(-halfWidth, halfHeight, -halfDepth), SpacePoint(-halfWidth, halfHeight, halfDepth)),
        Segment(SpacePoint(halfWidth, -halfHeight, -halfDepth), SpacePoint(halfWidth, -halfHeight, halfDepth)),
        Segment(SpacePoint(halfWidth, halfHeight, -halfDepth), SpacePoint(halfWidth, halfHeight, halfDepth)),
      )
      
  case class Cross(width: T,
                   height: T,
                   depth: T)
    extends Shape:

    val halfWidth = width / __2
    val halfHeight = height / __2
    val halfDepth = depth / __2

    override val segments: Segments =
      scala.Vector(
        Segment(SpacePoint(-halfWidth, __0, __0), SpacePoint(halfWidth, __0, __0)),
        Segment(SpacePoint(__0, -halfHeight, __0), SpacePoint(__0, halfHeight, __0)),
        Segment(SpacePoint(__0, __0, -halfDepth), SpacePoint(__0, __0, halfDepth))
      )
      
  object Cross:
    
    def Uniform(size: T): Cross =
      Cross(size, size, size)
      
    lazy val Normal: Cross =
      Uniform(__1)