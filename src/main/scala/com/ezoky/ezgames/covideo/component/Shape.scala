/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.component.Dimension.*

/**
 * @since 0.2.0
 * @author gweinbach on 31/05/2022
 */
trait Shape:
  val vertices: Vertices

  def translate(translationVector: Vector): Shape =
    Shape(
      vertices.map(_.translate(translationVector))
    )

  def rotate(rotationVector: Vector): Shape =
    Shape(
      vertices
//      vertices.map(_.rotate(rotationVector))
    )

type Vertices = Iterable[Vertex]

object Shape:
  def apply(shapeVertices: Vertices): Shape =
    new Shape:
      override val vertices = shapeVertices

case class Parallelepiped(box: Box)
  extends Shape:
  override val vertices: Vertices =
    val widthVector = box.width.vector
    val heightVector = box.height.vector
    val depthVector = box.depth.vector
    val diagonal = widthVector + heightVector + depthVector
    List(
      Vertex(Point.Zero, widthVector), Vertex(Point.Zero, heightVector), Vertex(Point.Zero, depthVector),
      Vertex(widthVector.dest, heightVector), Vertex(widthVector.dest, depthVector),
      Vertex(heightVector.dest, widthVector), Vertex(heightVector.dest, depthVector),
      Vertex(depthVector.dest, widthVector), Vertex(depthVector.dest, heightVector),
      Vertex(Point.Zero, -widthVector).translate(diagonal),
      Vertex(Point.Zero, -heightVector).translate(diagonal),
      Vertex(Point.Zero, -depthVector).translate(diagonal)
    )





