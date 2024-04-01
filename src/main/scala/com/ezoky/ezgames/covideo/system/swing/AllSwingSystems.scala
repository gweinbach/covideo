package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}


trait AllSwingSystems[I: Identifiable, D: Dimension]
  extends SceneWindows[I, D]
    with ControlWindows[I, D]
    with SceneControls[I, D]
    with SwingDisplaySystems[I, D]