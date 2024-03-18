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

  import CoordsDimension.{*, given}

  object Control:
    self =>

    private var model: ControlModel =
      ControlModel(
        ViewFrustumControl(),
        CameraControl()
      )

    private case class Callback(notifyMethod: () => Unit):
      def notifySubscriber: Unit =
        notifyMethod()

    private var subscribers: List[Callback] =
      List.empty

    private[swing] def subscribe(notifyMethod: () => Unit): Unit =
      subscribers = Callback(notifyMethod) :: subscribers

    private[swing] def getControl(controlledItem: ControlledItem): controlledItem.ItemControlType =
      model.control(controlledItem)

    private[swing] def updateControl(controlledItem: ControlledItem,
                                     update: controlledItem.ItemControlType => controlledItem.ItemControlType): Unit =
      self.model = model.updateControl(controlledItem, update)

    private[swing] def popModel(item: ControlledItem): ControlModel =
      val currentModel = self.model
      self.model = currentModel.acknowledgeUpdates(item)
      currentModel

    private[swing] def updateModel(newModel: ControlModel): Unit =
      if !newModel.equalsState(self.model) then
        self.model = newModel
        subscribers.foreach(_.notifySubscriber)
