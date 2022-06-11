package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Screen.*
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}

import spire.*
import spire.implicits.*
import spire.math.*

import java.util.UUID

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
case class Scene(id: SceneId = SceneId(),
                 name: String = "",
                 dimension: ScreenDimension = ScreenDimension.Empty,
                 margins: Margin = Margin(),
                 sprites: Population[Sprite] = Population.empty,
                 zoomRatio: ZoomRatio)
  extends Entity[SceneId] :

  lazy val preferredDimension: ScreenDimension =
    dimension withMargin margins

  // used to convert PositionValues to Pixels
  private given Numeric[PositionValue] = NumericPositionValue

  // This projection method should take care of 3D
  def project(position: Position): ScreenPosition =
    ScreenPosition(
      margins.left + (position.x.value.zoom(zoomRatio) px),
      margins.top + (position.y.value.zoom(zoomRatio) px)
    )

  // This projection method should take care of 3D
  def project(area: Box): ScreenDimension =
    ScreenDimension(
      area.maxPosition.x.value.zoom(zoomRatio) px,
      area.maxPosition.y.value.zoom(zoomRatio) px,
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

  def withMargins(margins: Margin): Scene =
    copy(margins = margins)


opaque type SceneId = UUID

object SceneId:
  def apply(): SceneId =
    UUID.randomUUID()

type ZoomRatio = Double


object DefaultScreenSize

type SceneSize = DefaultScreenSize.type | ScreenDimension

case class SceneConfig(name: String,
                       sceneSize: SceneSize = DefaultScreenSize,
                       margin: Margin = Margin(),
                       zoomRatio: ZoomRatio)
