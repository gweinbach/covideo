package com.ezoky.ezgames.covideo

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.component.Dimension.Geometry
import com.ezoky.ezgames.covideo.component.{AccelerationRange, SpeedRange}
import com.ezoky.ezgames.covideo.entity.{AreaConfig, GameConfig, Margin, PersonConfig, SceneConfig, SceneDimension, WorldConfig}

object Config:

  val Area: AreaConfig =
    AreaConfig(
      xGeometry = Geometry.Toric,
      yGeometry = Geometry.Toric,
      zGeometry = Geometry.Flat
    )
  val Scene: SceneConfig =
    SceneConfig(
      name = "COVIDEO",
//      sceneSize = SceneDimension(width = 1200 px, height = 800 px), // DefaultScreenSize
      sceneSize = SceneDimension(width = 800 px, height = 600 px), // DefaultScreenSize
//      margin = Margin(top = 100 px, left = 100 px, bottom = 100 px, right = 100 px)
    )
  val World: WorldConfig =
    WorldConfig(
      Area, Scene
    )
  val Person: PersonConfig =
    PersonConfig(
      initialSpeedRange = SpeedRange(-10.0 speed, 10.0 speed),
      accelerationRange = AccelerationRange(-2.5 acceleration, 2.5 acceleration)
    )
  val Game: GameConfig =
    GameConfig(
      populationSize = 1,
      Person, World
    )

  val Loop: GameLoopConfig =
    GameLoopConfig(
      fps = 60
    )