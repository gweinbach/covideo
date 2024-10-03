package com.ezoky.ezgames.covideo.system.javafx

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.system.{ControlModelStateHolder, Displays}
import spire.math.Numeric

trait ControlWindows[I: Identifiable, D: Dimension : Numeric]
  extends Displays[I, D]:

  private[javafx] class ControlWindow(control: ControlModelStateHolder)
  
  private[javafx] object ControlWindow:

    lazy val _ControlWindow = new ControlWindow(ControlModelStateHolder)

    def apply(): ControlWindow = _ControlWindow

