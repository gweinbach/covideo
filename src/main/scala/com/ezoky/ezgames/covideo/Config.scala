package com.ezoky.ezgames.covideo

import com.ezoky.ez3d.Screen.*
import com.ezoky.ezgames.covideo.MainConfig.*
import com.ezoky.ezgames.covideo.MainConfig.Everything.CoordsDimension.Ez3D.*
import com.ezoky.ezgames.covideo.MainConfig.Everything.CoordsDimension.{*, given}
import com.ezoky.ezgames.covideo.MainConfig.Everything.{*, given}
import com.ezoky.ezgames.covideo.component.Generate.generatedBetweenFractional
import spire.*
import spire.math.*

object Config:

  val AreaWidth = 800
  val AreaHeight = 800
  val AreaDepth = 800

  val NearDistance = 1500

  val PopulationSize = 100

  val Area =
    AreaConfig(
      AreaWidth size,
      xGeometry = Geometry.Toric,
      AreaHeight size,
      yGeometry = Geometry.Bounded,
      AreaDepth size,
      zGeometry = Geometry.Bounded
    )
  val Camera =
    CameraConfig(
      projectionType = ProjectionType.Perspective,
      position = PlanePoint(AreaWidth / 2, AreaHeight / 2),
      near = NearDistance,
      far = NearDistance + AreaDepth,
      top = AreaHeight / 2,
      right = AreaWidth / 2
    )
  val Scene =
    SceneConfig(
      name = "COVIDEO",
      sceneSize = ScreenDimension(width = 800 px, height = 800 px), // DefaultScreenSize
      //      sceneSize = DefaultScreenSize, //ScreenDimension(width = AreaWidth px, height = AreaHeight px), // DefaultScreenSize
      //      margin = Margin(top = 100 px, left = 100 px, bottom = 100 px, right = 100 px),
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
      shape = generatedBetweenFractional(10.0, 50.0).map(Cube(_)),
      solidConfig = Solid,
    )
  val Game =
    GameConfig(
      populationSize = PopulationSize,
      Person, World
    )

  // Control Config
  val GameControl =
    GameControlConfig(exitChar = 'x')
  val CameraControl =
    CameraControlConfig(10, 10, 10, 10)
  val ViewFrustumControl =
    ViewFrustumControlConfig(
      minNear = 1,
      maxNear = 2 * Camera.far,
      minFar = 1,
      maxFar = 2 * Camera.far,
      initialNear = Camera.near,
      initialFar = Camera.far
    )
  val UserControl =
    UserControlConfig(
      GameControl,
      CameraControl,
      ViewFrustumControl
    )

  val Loop =
    GameLoopConfig(
      fps = 60
    )
