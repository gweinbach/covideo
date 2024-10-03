package com.ezoky.ezgames.covideo

import com.ezoky.ez3d.Screen.*
import com.ezoky.ezgames.covideo.MainConfig.*
import com.ezoky.ezgames.covideo.MainConfig.Everything.CoordsDimension.Ez3D.*
import com.ezoky.ezgames.covideo.MainConfig.Everything.CoordsDimension.{*, given}
import com.ezoky.ezgames.covideo.MainConfig.Everything.{*, given}
import com.ezoky.ezgames.covideo.component.Generate.generatedBetweenFractional
import com.ezoky.ezgames.covideo.component.Rate
import com.ezoky.ezgames.covideo.component.Rate.*
import spire.*
import spire.math.*

object Config:

  // Area
  private val AreaWidth = 800
  private val AreaHeight = 800
  private val AreaDepth = 800

  // Camera
  private val NearDistance = 1500
  private val CameraType = ProjectionType.Perspective

  // Population
  private val PopulationSize = 1000

  val Area =
    AreaConfig(
      AreaWidth size,
      xGeometry = Geometry.Unbounded,
      AreaHeight size,
      yGeometry = Geometry.Bounded,
      AreaDepth size,
      zGeometry = Geometry.Bounded
    )
  val Camera =
    CameraConfig(
      projectionType = CameraType,
      position = PlanePoint(AreaWidth / 2, AreaHeight / 2),
      near = NearDistance,
      far = NearDistance + AreaDepth,
      top = AreaHeight / 2,
      right = AreaWidth / 2
    )
  val Scene =
    SceneConfig(
      name = "COVIDEO",
      sceneSize = ScreenDimension(width = 600 px, height = 600 px), // DefaultScreenSize
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
  val Demography =
    DemographyConfig(
      populationSize = PopulationSize,
      Person,
      birthRate = Rate(0.02),
      birthProfileDescription = List(1,49),
      deathRate = Rate(0.02),
      deathProfileDescription = List(0,33,1,33,1,32),
    )
  val Game =
    GameConfig(
      World,
      Demography
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
