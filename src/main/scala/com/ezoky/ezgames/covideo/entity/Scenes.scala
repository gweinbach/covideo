package com.ezoky.ezgames.covideo.entity

import com.ezoky.ez3d.Screen.*
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.{AllComponents, Identifiable, Dimension}

import spire.*
import spire.implicits.*
import spire.math.*


/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Scenes[I: Identifiable, D: Dimension]
  extends Entities[I]
  with AllComponents[D]:

  import CoordsDimension.{*, given}
  import CoordsDimension.Ez3D.*
    
  case class Scene(id: I = summon[Identifiable[I]].id,
                   name: String = "",
                   dimension: ScreenDimension = ScreenDimension.Empty,
                   margins: Margin = Margin(),
                   sprites: Population[Sprite] = Population.empty,
                   components: Population[Component3D] = Population.empty,
                   camera: Camera,
                   zoomRatio: ZoomRatio)
    extends Entity:
  
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
  
    def withSprite(id: I,
                   sprite: Sprite): Scene =
      copy(sprites = sprites + (id -> sprite))
  
    def withSprites(sprites: Population[Sprite]): Scene =
      copy(sprites = sprites ++ sprites)
  
    def withComponent(id: I,
                      component: Component3D): Scene =
      copy(components = components + (id -> component))
  
    def withComponents(components: Population[Component3D]): Scene =
      copy(components = components ++ components)
  
    def withMargins(margins: Margin): Scene =
      copy(margins = margins)
  
    def withCamera(camera: Camera): Scene =
      copy(camera = camera)

  
  type ZoomRatio = Double
  
  
  object DefaultScreenSize
  
  type SceneSize = DefaultScreenSize.type | ScreenDimension
  
  case class SceneConfig(name: String,
                         sceneSize: SceneSize = DefaultScreenSize,
                         margin: Margin = Margin(),
                         zoomRatio: ZoomRatio,
                         camera: CameraConfig)
  
  case class CameraConfig(position: PlanePoint,
                          near: DimensionBase,
                          far: DimensionBase,
                          top: DimensionBase,
                          right: DimensionBase)
  
  given WindowView[Scene] with
    extension (scene: Scene)
      // Scene is projected on a square between -1 & 1
      def windowOrigin: PlanePoint = PlanePoint(MinusOne, MinusOne) // TopLeft = PlanePoint(-1, 1)
      def flipX: Boolean = false
      def flipY: Boolean = false
      def flipZ: Boolean = true
      def screenDimension: ScreenDimension = scene.dimension
  
  
  case class Component3D(position: SpacePoint,
                         basis: Basis,
                         shape: Shape)
  
  given ComponentModel[Component3D] with
    extension (component: Component3D)
      override def position = component.position
      override def basis = component.basis
      override def shape: Shape = component.shape
  
