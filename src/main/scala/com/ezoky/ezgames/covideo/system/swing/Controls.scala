/*
 * @author gweinbach on 16/07/2022 22:45
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.system.Displays

import reflect.Selectable.reflectiveSelectable

/**
 * @since 0.2.0
 * @author gweinbach on 16/07/2022
 */
trait Controls[I: Identifiable, D: Dimension]
  extends Displays[I, D]:
  
  object Control:
    self =>
  
    private[swing] var model: ControlModel =
      ControlModel(
        ViewFrustumControl(0, 0, 0),
        CameraControl()
      )
  
    case class Callback(notifyMethod: () => Unit):
      def notifySubscriber: Unit =
        notifyMethod()
  
    private var subscribers: List[Callback] =
      List.empty
  
    def subscribe(notifyMethod: () => Unit): Unit =
      subscribers = Callback(notifyMethod) :: subscribers
  
    private[swing] def setModel(newModel: ControlModel): Unit =
      self.model = newModel
  
    def popModel(item: ControlledItem): ControlModel =
      val currentModel = self.model
      self.model = currentModel.acknowledgeUpdates(item)
      currentModel
  
    def updateModel(newModel: ControlModel): Unit =
      if !newModel.equalsState(self.model) then
        self.model = newModel
        subscribers.foreach(_.notifySubscriber)
  //      println(s"modelA: near=${self.model.viewFrustum.near}, far=${self.model.viewFrustum.far}")
  //      _nearSlider.notifyChange()
  //      println(s"modelB: near=${self.model.viewFrustum.near}, far=${self.model.viewFrustum.far}")
  //      _farSlider.notifyChange()
  //      println(s"modelC: near=${self.model.viewFrustum.near}, far=${self.model.viewFrustum.far}")
