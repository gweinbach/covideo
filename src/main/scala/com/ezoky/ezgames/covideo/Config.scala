package com.ezoky.ezgames.covideo

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Screen.*
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.entity.People.*

object Config:

  val Area =
    AreaConfig(
      800 size,
      xGeometry = Geometry.Toric,
      600 size,
      yGeometry = Geometry.Toric,
      200 size,
      zGeometry = Geometry.Toric
    )
  val Scene =
    SceneConfig(
      name = "COVIDEO",
//      sceneSize = SceneDimension(width = 1200 px, height = 800 px), // DefaultScreenSize
      sceneSize = ScreenDimension(width = 800 px, height = 600 px), // DefaultScreenSize
//      margin = Margin(top = 100 px, left = 100 px, bottom = 100 px, right = 100 px)
      zoomRatio = 1.5
    )
  val World =
    WorldConfig(
      Area, Scene
    )
  val Mobile = MobileConfig(
    speedRange = SpeedRange(-3.0 speed, 3.0 speed),
    accelerationRange = AccelerationRange(-1.5 acceleration, 1.5 acceleration)
  )
  val Person =
    PersonConfig(
      mobileConfig = Mobile
    )
  val Game =
    GameConfig(
      populationSize = 20,
      Person, World
    )
  val Loop =
    GameLoopConfig(
      fps = 60
    )