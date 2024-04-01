/*
 * @author gweinbach on 16/07/2022 22:45
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.system.Displays

/**
 * @since 0.2.0
 * @author gweinbach on 16/07/2022
 */
trait SceneControls[I: Identifiable, D: Dimension]
  extends Displays[I, D]:

  import CoordsDimension.given

  class SceneControl(initialModel: ControlModel):
    self =>

    private var model: ControlModel = initialModel

    private case class Callback(notifyMethod: () => Unit):
      def notifySubscriber: Unit =
        notifyMethod()

    private var subscribersToUpdate: List[Callback] =
      List.empty

    private[swing] def subscribeToUpdates(notifyMethod: () => Unit): Unit =
      subscribersToUpdate = Callback(notifyMethod) :: subscribersToUpdate

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
        subscribersToUpdate.foreach(_.notifySubscriber)


  object SceneControl
    extends SceneControl(ControlModel(
      GameControl(),
      ViewFrustumControl(),
      CameraControl()
    ))