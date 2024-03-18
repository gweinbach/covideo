
package com.ezoky.ezgames.covideo

import com.ezoky.ez3d.Screen.*
import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.HealthCondition.*
import com.ezoky.ezgames.covideo.entity.Builder

import MainConfig.{*, given}
import MainConfig.Everything.{*, given}
import MainConfig.Everything.CoordsDimension.{*, given}
import MainConfig.Everything.CoordsDimension.Ez3D.*

/**
* @author gweinbach on 03/01/2022
* @since 0.2.0
*/
case class GameBuilder(gameConfig: GameConfig)
                      (using displaySystem: DisplaySystem)
  extends Builder[Game]:

  override def build: Generated[Game] =
    for
      world <- WorldBuilder(gameConfig.worldConfig).build
      people <- Generated.setOf(PersonBuilder(world.area, gameConfig.personConfig).build)(gameConfig.populationSize)
    yield
      Game(
        world,
        Population(people)
      )


case class WorldBuilder(worldConfig: WorldConfig)
                       (using displaySystem: DisplaySystem)
  extends Builder[World]:

  override def build: Generated[World] =
    for
      area <- AreaBuilder(
        worldConfig.areaConfig
      ).build
      scene <- SceneBuilder(
        worldConfig.sceneConfig,
      ).build
    yield
      World(
        area,
        scene
      )


private case class AreaBuilder(areaConfig: AreaConfig)
  extends Builder[Box]:

  override def build: Generated[Box] =
    Generated(
      Box(
        Width(areaConfig.width)(using areaConfig.xGeometry),
        Height(areaConfig.height)(using areaConfig.yGeometry),
        Depth(areaConfig.depth)(using areaConfig.zGeometry),
      )
    )

private case class SceneBuilder(sceneConfig: SceneConfig)
                               (using displaySystem: DisplaySystem)
  extends Builder[Scene]:

  lazy val sceneDimension =
    sceneConfig.sceneSize match
      case DefaultScreenSize =>
        displaySystem.defaultScreenSceneDimension
      case sceneSize: ScreenDimension =>
        sceneSize


  override def build: Generated[Scene] =
    for
      camera <- CameraBuilder(sceneConfig.camera).build
    yield
      Scene(
        name = sceneConfig.name,
        dimension = sceneDimension,
        margins = sceneConfig.margin,
        zoomRatio = sceneConfig.zoomRatio,
        camera = camera
      )


case class CameraBuilder(cameraConfig: CameraConfig)
  extends Builder[Camera]:

  override def build: Generated[Camera] =
    Generated.unit(
      Orthographic.viewBoxFromLeft(
        sceneDepth = cameraConfig.right * 2,
        sceneHeight = cameraConfig.top * 2,
        sceneWidth = cameraConfig.far - cameraConfig.near,
        cameraDistance = cameraConfig.near
      ).getOrElse(Camera.Default)
      //        Orthographic.viewBoxFromTop(
      //      sceneWidth = cameraConfig.right * 10,
      //      sceneHeight = cameraConfig.top * 10,
      //      sceneDepth = cameraConfig.far - cameraConfig.near,
      //      cameraDistance = cameraConfig.near
      //    ).getOrElse(Camera.Default)
    )


case class MobileBuilder(area: Box,
                         mobileConfig: MobileConfig)
  extends Builder[Mobile]:

  override def build: Generated[Mobile] =
    for
      position <- area.generatedPosition
      speed <- Speed.generated(mobileConfig.speedRange, mobileConfig.speedRange, mobileConfig.speedRange)
      acceleration <- Acceleration.generated(mobileConfig.accelerationRange, mobileConfig.accelerationRange, mobileConfig.accelerationRange)
    yield
      Mobile(
        position,
        area,
        speed,
        mobileConfig.speedRange,
        acceleration,
        mobileConfig.accelerationRange
      )

case class SolidBuilder(area: Box,
                        solidConfig: SolidConfig)
  extends Builder[Solid]:

  override def build: Generated[Solid] =
    for
      mobile <- MobileBuilder(area, solidConfig.mobileConfig).build
      spin <- Spin.generated(solidConfig.spinRange, solidConfig.spinRange, solidConfig.spinRange)
      angularAcceleration <- AngularAcceleration.generated(solidConfig.angularAccelerationRange, solidConfig.angularAccelerationRange, solidConfig.angularAccelerationRange)
    yield
      Solid(
        mobile,
        Basis.NormalDirect,
        spin,
        solidConfig.spinRange,
        angularAcceleration,
        solidConfig.angularAccelerationRange
      )


case class PersonBuilder(area: Box,
                         personConfig: PersonConfig)
                        (using displaySystem: DisplaySystem)
  extends Builder[Person]:

  override def build: Generated[Person] =
    for
      solid <- SolidBuilder(area, personConfig.solidConfig).build
      shape <- personConfig.shape
    yield
      Person(
        solid = solid,
        healthCondition = Healthy,
        sprite = displaySystem.spriteByHealthCondition(Healthy),
        shape = shape
      )


