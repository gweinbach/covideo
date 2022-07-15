package com.ezoky.ezgames.covideo

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ez3d.Screen.*
import com.ezoky.ezgames.covideo.component.Generate.generatedBetweenFractional
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.entity.People.*

object Config:
  
  val AreaWidth = 1200
  val AreaHeight = 1000
  val AreaDepth = 1200
  
  val NearDistance = 1500

  val Area =
    AreaConfig(
      AreaWidth size,
      xGeometry = Geometry.Bounded,
      AreaHeight size,
      yGeometry = Geometry.Bounded,
      AreaDepth size,
      zGeometry = Geometry.Bounded
    )
  val Camera =
    CameraConfig(
      position = PlanePoint(AreaWidth / 2, AreaHeight / 2),
      near = NearDistance,
      far = NearDistance + AreaDepth,
      top = AreaHeight / 2,
      right = AreaWidth / 2
    )
  val Scene =
    SceneConfig(
      name = "COVIDEO",
//      sceneSize = SceneDimension(width = 1200 px, height = 800 px), // DefaultScreenSize
      sceneSize = ScreenDimension(width = AreaWidth px, height = AreaHeight px), // DefaultScreenSize
//      margin = Margin(top = 100 px, left = 100 px, bottom = 100 px, right = 100 px)
      zoomRatio = 1.0, // not used in 3D
      camera = Camera
    )
  val World =
    WorldConfig(
      Area, Scene
    )
  val Mobile = MobileConfig(
    speedRange = SpeedRange(-3.0 speed, 3.0 speed),
    accelerationRange = AccelerationRange(-1.5 acceleration, 1.5 acceleration)
  )
  val Solid =
    SolidConfig(
      mobileConfig = Mobile,
      spinRange = SpinRange(-0.1 spin, 0.1 spin),
      angularAccelerationRange = AngularAccelerationRange(-0.05 angularAcceleration, 0.05 angularAcceleration)
    )
  val Person =
    PersonConfig(
      shape = generatedBetweenFractional(10.0,50.0).map(Cube(_)),
      solidConfig = Solid,
    )
  val Game =
    GameConfig(
      populationSize = 500,
      Person, World
    )
  val Loop =
    GameLoopConfig(
      fps = 60
    )