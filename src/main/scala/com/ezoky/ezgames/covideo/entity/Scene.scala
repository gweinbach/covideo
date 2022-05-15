package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Scene:

  lazy val preferredDimension: SceneDimension

  def project(position: Position): ScenePosition
  
  def project(area: Area): SceneDimension

  def withSprite(sprite: Sprite): Scene

  def withSprites(sprites: Iterable[Sprite]): Scene =
    sprites.foldLeft(this)(
      (scene, sprite) =>
        scene.withSprite(sprite)
    )

  def withArea(area: Area): Scene



opaque type Pixel = Int

extension (pixel: Pixel)
  def px: Pixel = pixel

  def +(other: Pixel): Pixel =
    pixel + other

  def -(other: Pixel): Pixel =
    pixel - other

  def width(using Geometry: Geometry): Width =
    Width(pixel size)

  def height(using Geometry: Geometry): Height =
    Height(pixel size)

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
      width - margin.left - margin.right,
      height - margin.top - margin.bottom
    )

case class ScenePosition(x: Pixel,
                         y: Pixel)

case class Margin(top: Pixel = 0 px,
                  left: Pixel = 0 px,
                  bottom: Pixel = 0 px,
                  right: Pixel = 0 px)


object DefaultScreenSize
type SceneSize = DefaultScreenSize.type | SceneDimension

case class SceneConfig(name: String,
                       sceneSize: SceneSize = DefaultScreenSize,
                       margin: Margin = Margin())