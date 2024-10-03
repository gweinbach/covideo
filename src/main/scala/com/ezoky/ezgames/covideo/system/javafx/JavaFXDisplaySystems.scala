package com.ezoky.ezgames.covideo.system.javafx

import com.ezoky.ez3d.Screen
import com.ezoky.ez3d.Screen.{Pixel, ScreenDimension, px}
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.javafx.JavaFXSprites
import com.ezoky.ezgames.covideo.component.{Dimension, HealthCondition, Identifiable, Sprites}
import com.ezoky.ezgames.covideo.entity.{Entities, Scenes}
import com.ezoky.ezgames.covideo.system.{ControlModel, ControlledItem, Displays}
import javafx.application.Platform

trait JavaFXDisplaySystems[I: Identifiable, D: Dimension]
  extends SceneWindows[I, D]
    with Displays[I, D]
    with Scenes[I, D]
    with Entities[I]
    with Sprites[D]
    with JavaFXSprites[D]:

  class JavaFXDisplaySystem(userControlConfig: UserControlConfig)
    extends DisplaySystem(userControlConfig):

    override def defaultScreenSceneDimension: Screen.ScreenDimension =
      import javafx.stage.Screen

      val screenBounds = Screen.getPrimary().getBounds()

      val screenWidth: Pixel = screenBounds.getWidth px
      val screenHeight: Pixel = screenBounds.getHeight px

      ScreenDimension(screenWidth, screenHeight)

    override def displayControls(): IO[Unit] =
      IO {}

    override def displayScene(scene: Scene): IO[Unit] =
      IO {
        val sceneWindow = SceneWindow(scene.id, userControlConfig)
        if !sceneWindow.isDisplayed then
//          given ViewFrustumControlConfig = userControlConfig.viewFrustumConfig
//
//          SceneControl.updateControl(ControlledItem.ViewFrustum, _.control(LifeCycleEvent.DisplayScene))

          sceneWindow.display()

        // side effects, not pure
        sceneWindow.updateTitle(scene.name)
        sceneWindow.resizeScene(scene.preferredDimension)
        sceneWindow.draw(scene)
      }

    override def dispose(doDispose: Boolean): IO[Unit] =
      IO {
        if doDispose then
          Platform.exit()
      }
