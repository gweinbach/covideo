package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}

import scala.math.Numeric.DoubleIsFractional

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
case class Scene(area: Area,
                 name: String = "",
                 margins: Margin = Margin(),
                 sprites: Population[Sprite] = Population.empty):

  lazy val preferredDimension: SceneDimension =
    project(area) withMargin margins

  // This projection method should take care of 3D
  def project(position: Position): ScenePosition =
    ScenePosition(
      margins.left + position.x.value.intValue px,
      margins.top + position.y.value.intValue px
    )

  // This projection method should take care of 3D
  def project(area: Area): SceneDimension =
    SceneDimension(
      area.maxPosition.x.value.intValue px,
      area.maxPosition.y.value.intValue px,
    )

  def withName(name: String): Scene =
    copy(name = name)

  def withSprite(id: PersonId,
                 sprite: Sprite): Scene =
    copy(sprites = sprites.add(id -> sprite))

  def withSprites(sprites: Population[Sprite]): Scene =
    sprites.foldLeft(this) {
      case (scene, (id: PersonId, sprite)) =>
        scene.withSprite(id, sprite)
    }

  def withArea(area: Area): Scene =
    copy(area = area)

  def withMargins(margins: Margin): Scene =
    copy(margins = margins)


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

case class ScenePosition(x: Pixel,
                         y: Pixel)

case class Margin(top: Pixel = 0 px,
                  left: Pixel = 0 px,
                  bottom: Pixel = 0 px,
                  right: Pixel = 0 px)

