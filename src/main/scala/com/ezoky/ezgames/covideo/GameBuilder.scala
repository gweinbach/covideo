/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *  
 */

/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ezgames.covideo

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.HealthCondition.*
import com.ezoky.ezgames.covideo.entity.People.*
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.system.DisplaySystem
import com.ezoky.ezgames.covideo.system.swing.*

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
case class GameBuilder(gameConfig: GameConfig)
                      (using DisplaySystem)
  extends Builder[Game] :

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
  extends Builder[World] :

  lazy val sceneDimension =
    worldConfig.sceneConfig.sceneSize match
      case DefaultScreenSize =>
        summon[DisplaySystem].defaultScreenSceneDimension
      case sceneSize: SceneDimension =>
        sceneSize

  override def build: Generated[World] =
    for
      area <- AreaBuilder(
        sceneDimension,
        worldConfig.areaConfig
      ).build
      scene <- SceneBuilder(
        area,
        worldConfig.sceneConfig,
      ).build
    yield
      World(
        area,
        scene
      )


private case class AreaBuilder(sceneDimension: SceneDimension,
                               areaConfig: AreaConfig)
  extends Builder[Area] :

  override def build: Generated[Area] =
    Generated(
      Area(
        sceneDimension.width.width(using areaConfig.xGeometry),
        sceneDimension.height.height(using areaConfig.yGeometry),
        Depth.Flat
      )
    )

private case class SceneBuilder(area: Area,
                                sceneConfig: SceneConfig)
  extends Builder[Scene] :

  override def build: Generated[Scene] =
    Generated(
      Scene(
        area,
        name = sceneConfig.name,
        margins = sceneConfig.margin
      )
    )


case class MobileBuilder(area: Area,
                         mobileConfig: MobileConfig)
  extends Builder[Mobile] :

  override def build: Generated[Mobile] =
    for
      position <- Position.generated(area)
      speed <- Speed.generated(mobileConfig.speedRange, mobileConfig.speedRange, mobileConfig.speedRange)
      acceleration <- Acceleration.generated(mobileConfig.accelerationRange, mobileConfig.accelerationRange, mobileConfig.accelerationRange)
    yield
      Mobile(
        position,
        speed,
        mobileConfig.speedRange,
        acceleration,
        mobileConfig.accelerationRange
      )

case class PersonBuilder(area: Area,
                         personConfig: PersonConfig)
                        (using DisplaySystem)
  extends Builder[Person] :

  override def build: Generated[Person] =
    for
      mobile <- MobileBuilder(area, personConfig.mobileConfig).build
    yield
      Person(
        id = PersonId(),
        mobile,
        Healthy,
        summon[DisplaySystem].spriteByHealthCondition(Healthy)
      )


