package com.ezoky.ezgames.covideo

import com.ezoky.ezgames.covideo.component.swing.AllSwingComponents
import com.ezoky.ezgames.covideo.component.javafx.AllJavaFXComponents
import com.ezoky.ezgames.covideo.component.{AllComponents, Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.AllEntities
import com.ezoky.ezgames.covideo.system.AllSystems
import com.ezoky.ezgames.covideo.system.javafx.AllJavaFXSystems
import com.ezoky.ezgames.covideo.system.swing.AllSwingSystems

trait GameBootstrap[I: Identifiable, D: Dimension]
  extends AllComponents[D]
    with AllEntities[I, D]
    with AllSystems[I, D]:

  def displaySystem(userControlConfig: UserControlConfig): DisplaySystem 


trait SwingGameBootstrap[I: Identifiable, D: Dimension]
  extends GameBootstrap[I, D]
    with AllSwingComponents[D]
    with AllSwingSystems[I, D]:

  override def displaySystem(userControlConfig: UserControlConfig): DisplaySystem =
    SwingDisplaySystem(userControlConfig)


trait JavaFXGameBootstrap[I: Identifiable, D: Dimension]
  extends GameBootstrap[I, D]
    with AllJavaFXComponents[D]
    with AllJavaFXSystems[I, D]:

  override def displaySystem(userControlConfig: UserControlConfig): DisplaySystem =
    JavaFXDisplaySystem(userControlConfig)