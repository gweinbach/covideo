package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ez3d.Screen.{Pixel, ScreenDimension, px}
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.swing.SwingSprite
import com.ezoky.ezgames.covideo.component.{HealthCondition, Sprite}
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}
import com.ezoky.ezgames.covideo.entity.{Scene, given}
import com.ezoky.ezgames.covideo.system.{ControlModel, DisplaySystem}

/**
 * @author gweinbach on 14/05/2022 12:29
 * @since 0.2.0
 */
object SwingDisplaySystem
  extends DisplaySystem :

  override def defaultScreenSceneDimension: ScreenDimension =
    import java.awt.{GraphicsDevice, GraphicsEnvironment}

    val gd: GraphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
    val screenWidth: Pixel = gd.getDisplayMode.getWidth px
    val screenHeight: Pixel = gd.getDisplayMode.getHeight px

    ScreenDimension(screenWidth, screenHeight)


  override def controlModel: ControlModel =
    ControlWindow().model

  override def initUI(): IO[Unit] =
    IO {
      ControlWindow().display
    }

  override def drawScene(scene: Scene): IO[Unit] =
    IO {
      // side effects, not pure
      val mainWindow = SceneWindow(scene.id)
      mainWindow.updateTitle(scene.name)
      mainWindow.resize(scene.preferredDimension)
      mainWindow.draw(scene)
    }

  override def spriteByHealthCondition(healthCondition: HealthCondition): Sprite =
    healthCondition match
      case HealthCondition.Healthy => SwingSprite.SmileySunglasses
      case HealthCondition.Sick => SwingSprite.SmileySick
