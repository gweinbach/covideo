package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}

import scala.math.Numeric.DoubleIsFractional

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Scene:

  lazy val preferredDimension: SceneDimension

  def project(position: Position): ScenePosition

  def project(area: Area): SceneDimension

  def withSprite(id: PersonId,
                 sprite: Sprite): Scene

  def withSprites(sprites: Population[Sprite]): Scene =
    sprites.foldLeft(this) {
      case (scene, (id: PersonId, sprite)) =>
        scene.withSprite(id, sprite)
    }

  def withArea(area: Area): Scene


object DefaultScreenSize

type SceneSize = DefaultScreenSize.type | SceneDimension

case class SceneConfig(name: String,
                       sceneSize: SceneSize = DefaultScreenSize,
                       margin: Margin = Margin())

opaque type Pixel = Int

extension (pixel: Pixel)
  def px: Pixel = pixel

  def +(other: Pixel): Pixel =
    pixel + other

  def -(other: Pixel): Pixel =
    pixel - other

  //  def size(viewScale: Double): SizeValue =
  //    (pixel size).zoom(1.0 / viewScale)(using DoubleIsFractional)

  def width(using Geometry: Geometry): Width =
    Width(pixel size)

  def height(using Geometry: Geometry): Height =
    Height(pixel size)

  def depth(using Geometry: Geometry): Depth =
    Depth(pixel size)

  def asInt: Int =
    pixel

case class SceneDimension(width: Pixel,
                          height: Pixel):
  def withMargin(margin: Margin): SceneDimension =
    SceneDimension(
      width + margin.left + margin.right,
      height + margin.top + margin.bottom
    )

  def withoutMargin(margin: Margin): SceneDimension =
    SceneDimension(
      width,
      height
    )

case class ScenePosition(x: Pixel,
                         y: Pixel)

case class Margin(top: Pixel = 0 px,
                  left: Pixel = 0 px,
                  bottom: Pixel = 0 px,
                  right: Pixel = 0 px)

