
package com.ezoky.ezgames.covideo

import com.ezoky.ez3d.Screen.*
import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.HealthCondition.*
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.entity.People.*
import com.ezoky.ezgames.covideo.system.DisplaySystem
import com.ezoky.ezgames.covideo.system.swing.*

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
case class GameBuilder(gameConfig: GameConfig)
                      (using DisplaySystem)
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
                       (using DisplaySystem)
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
                               (using DisplaySystem)
  extends Builder[Scene]:

  lazy val sceneDimension =
    sceneConfig.sceneSize match
      case DefaultScreenSize =>
        summon[DisplaySystem].defaultScreenSceneDimension
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
      (
        for
          viewFrustum <- Perspective.ViewFrustum.fromSymetricPlanes(
            nearDistance = cameraConfig.near,
            farDistance = cameraConfig.far,
            topDistance = cameraConfig.top,
            rightDistance = cameraConfig.right
          )
          camera <- Perspective.LookAtCamera.safe(
            position = cameraConfig.position.withZ(cameraConfig.near),
            target = cameraConfig.position.withZ(0), // always looking towards center of near plan
            upVector = SpaceVector.OneY,
            viewFrustum = viewFrustum
          )
        yield
          camera
      ).getOrElse(Camera.Default)
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
    yield
      Solid(
        mobile,
        Basis.Normal,
        spin
      )


case class PersonBuilder(area: Box,
                         personConfig: PersonConfig)
                        (using DisplaySystem)
  extends Builder[Person]:

  override def build: Generated[Person] =
    for
      solid <- SolidBuilder(area, personConfig.solidConfig).build
    yield
      Person(
        id = PersonId(),
        solid,
        Healthy,
        summon[DisplaySystem].spriteByHealthCondition(Healthy),
        Cube(40)
      )


