package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.system.Builder

import java.awt.{Dimension as AWTDimension, EventQueue as AWTEventQueue}
import java.util.UUID
import javax.swing.{JFrame, JPanel}

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
case class GameBuilder(gameConfig: GameConfig)
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
  extends Builder[World] :

  override def build: Generated[World] =

    val sceneDimension =
      worldConfig.sceneConfig.sceneSize match
        case DefaultScreenSize =>
          DefaultScreenSceneDimension
        case sceneSize: SceneDimension =>
          sceneSize

    for
      area <- AreaBuilder(
        sceneDimension withoutMargin worldConfig.sceneConfig.margin,
        worldConfig.areaConfig
      ).build
      scene <- SwingSceneBuilder(
        worldConfig.sceneConfig,
        area
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
    Generated.unit(
      Area(
        sceneDimension.width.width(using areaConfig.xGeometry),
        sceneDimension.height.height(using areaConfig.yGeometry),
        Depth.Flat
      )
    )

case class PersonBuilder(area: Area,
                         personConfig: PersonConfig)
  extends Builder[Person] :

  override def build: Generated[Person] =
    for
      position <- Position.generated(area)
      speed <- Speed.generated(personConfig.initialSpeedRange, personConfig.initialSpeedRange, personConfig.initialSpeedRange)
      acceleration <- Acceleration.generated(personConfig.accelerationRange, personConfig.accelerationRange, personConfig.accelerationRange)
    yield
      Person(
        id = PersonId(),
        position,
        speed,
        personConfig.initialSpeedRange,
        acceleration,
        personConfig.accelerationRange,
        Healthy,
        SwingSprite(Assets.SmileySunglasses, position)
      )


private val DefaultScreenSceneDimension: SceneDimension =

  import java.awt.{GraphicsDevice, GraphicsEnvironment}

  val gd: GraphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
  val screenWidth: Pixel = gd.getDisplayMode.getWidth px
  val screenHeight: Pixel = gd.getDisplayMode.getHeight px

  SceneDimension(screenWidth, screenHeight)




given Conversion[Pixel, Int] with
  def apply(pixel: Pixel): Int = pixel.asInt

extension (scenePosition: ScenePosition)
  def awtDimension: AWTDimension =
    AWTDimension(scenePosition.x, scenePosition.y)

extension (sceneDimension: SceneDimension)
  def awtDimension: AWTDimension =
    AWTDimension(sceneDimension.width, sceneDimension.height)