package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ez3d.Screen.{Pixel, ScreenDimension, px}
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.swing.SwingSprites
import com.ezoky.ezgames.covideo.component.{Dimension, HealthCondition, Identifiable, Sprites}
import com.ezoky.ezgames.covideo.entity.{Entities, Scenes, given}
import com.ezoky.ezgames.covideo.system.{Displays, LifeCycleEvent, ControlledItem, ControlModel}

/**
 * @author gweinbach on 14/05/2022 12:29
 * @since 0.2.0
 */
trait SwingDisplaySystems[I: Identifiable, D: Dimension]
  extends SceneWindows[I, D]
    with ControlWindows[I, D]
    with SceneControls[I, D]
    with Displays[I, D]
    with Scenes[I, D]
    with Entities[I]
    with Sprites[D]
    with SwingSprites[D]:

  class SwingDisplaySystem(userControlConfig: UserControlConfig)
    extends DisplaySystem(userControlConfig):

    override def defaultScreenSceneDimension: ScreenDimension =
      import java.awt.{GraphicsDevice, GraphicsEnvironment}

      val gd: GraphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
      val screenWidth: Pixel = gd.getDisplayMode.getWidth px
      val screenHeight: Pixel = gd.getDisplayMode.getHeight px

      ScreenDimension(screenWidth, screenHeight)


    override def popControlModel(item: ControlledItem): IO[ControlModel] =
      IO {
        SceneControl.popModel(item)
      }

    override def updateControlModel(model: ControlModel): IO[Unit] =
      IO {
        SceneControl.updateModel(model)
      }

    override def displayControls(): IO[Unit] =
      IO {
        val controlWindow = ControlWindow()
        if !controlWindow.isVisible then
          given ViewFrustumControlConfig = userControlConfig.viewFrustumConfig
          SceneControl.updateControl(ControlledItem.ViewFrustum, _.control(LifeCycleEvent.DisplayControls))

          controlWindow.display()
      }

    override def displayScene(scene: Scene): IO[Unit] =
      IO {
        val sceneWindow = SceneWindow(scene.id, userControlConfig)
        if !sceneWindow.isVisible then
          given ViewFrustumControlConfig = userControlConfig.viewFrustumConfig
          SceneControl.updateControl(ControlledItem.ViewFrustum, _.control(LifeCycleEvent.DisplayScene))

          sceneWindow.display()
          
        // side effects, not pure
        sceneWindow.updateTitle(scene.name)
        sceneWindow.resizeScene(scene.preferredDimension)
        sceneWindow.draw(scene)
      }

    override def dispose(doDispose: Boolean): IO[Unit] =
      IO {
        if doDispose then

          given ViewFrustumControlConfig = userControlConfig.viewFrustumConfig

          SceneControl.updateControl(ControlledItem.ViewFrustum, _.control(LifeCycleEvent.DisposeControls))
          ControlWindow().dispose()

          SceneControl.updateControl(ControlledItem.ViewFrustum, _.control(LifeCycleEvent.DisplayScene))
          SceneWindow.all().map(_.dispose())
      }

    override def spriteByHealthCondition(healthCondition: HealthCondition): Sprite =
      healthCondition match
        case HealthCondition.Healthy => SwingSprite.SmileySunglasses
        case HealthCondition.Sick => SwingSprite.SmileySick
        case _ => SwingSprite.SmileySunglasses
