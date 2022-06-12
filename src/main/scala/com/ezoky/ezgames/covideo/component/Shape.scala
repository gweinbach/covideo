/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ez3d.{Rotation, Transformable, Translation}
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*

/**
 * @since 0.2.0
 * @author gweinbach on 31/05/2022
 */
trait Shape
  extends Transformable[Shape] :
  val vertices: Vertices


class ShapeRotation(using Rotation[Vertex])
  extends Rotation[Shape] :

  override def rotate(v: Shape): Option[Shape] =
    v.vertices.map(_.rotate).foldLeft[Option[scala.Vector[Vertex]]](Some(scala.Vector.empty[Vertex])) {
      case (Some(vertices), Some(vertex)) =>
        Some(vertices :+ vertex)
      case _ =>
        None
    }.map(Shape.apply)


class ShapeTranslation(using Translation[Vertex])
  extends Translation[Shape] :

  override def translate(v: Shape): Shape =
    Shape(v.vertices.map(_.translate))


object Shape:
  def apply(shapeVertices: Vertices): Shape =
    new Shape :
      override val vertices = shapeVertices

case class Parallelepiped(box: Box)
  extends Shape :
  override val vertices: Vertices =
    val widthVector = box.width.vector
    val heightVector = box.height.vector
    val depthVector = box.depth.vector
    val diagonal = VertexTranslation(widthVector + heightVector + depthVector)
    scala.Vector(
      Vertex(Point.Zero, widthVector), Vertex(Point.Zero, heightVector), Vertex(Point.Zero, depthVector),
      Vertex(widthVector.dest(), heightVector), Vertex(widthVector.dest(), depthVector),
      Vertex(heightVector.dest(), widthVector), Vertex(heightVector.dest(), depthVector),
      Vertex(depthVector.dest(), widthVector), Vertex(depthVector.dest(), heightVector),
      Vertex(Point.Zero, -widthVector).translate(using diagonal),
      Vertex(Point.Zero, -heightVector).translate(using diagonal),
      Vertex(Point.Zero, -depthVector).translate(using diagonal)
    )





