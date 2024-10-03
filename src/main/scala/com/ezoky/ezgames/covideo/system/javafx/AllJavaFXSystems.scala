package com.ezoky.ezgames.covideo.system.javafx

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}

trait AllJavaFXSystems[I: Identifiable, D: Dimension]
  extends JavaFXDisplaySystems[I, D]
    with SceneWindows[I, D]
    with ControlWindows[I, D]