/*
 * @author gweinbach on 30/06/2022 23:38
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.Dimension.DimensionBase
import com.ezoky.ezgames.covideo.system.{CameraControl, ControlModel}

import java.awt.{BorderLayout, Graphics2D, GraphicsDevice, GraphicsEnvironment, GridLayout, Color as AWTColor, Dimension as AWTDimension, EventQueue as AWTEventQueue}
import javax.swing.*
import javax.swing.event.{ChangeEvent, ChangeListener}

/**
 * @since 0.2.0
 * @author gweinbach on 30/06/2022
 */
private class ControlWindow()
  extends JFrame :

  self =>

  println("Creating a ControlWindow")

  private var model: ControlModel = ControlModel(CameraControl(0, 0, 0))

  def popModel(): ControlModel =
    val currentModel = self.model
    self.model = currentModel.acknowledgeUpdates()
    currentModel

  def updateModel(newModel: ControlModel): Unit =
    if !newModel.equalsState(self.model) then
      self.model = newModel.acknowledgeUpdates()
      println(s"modelA: near=${self.model.camera.near}, far=${self.model.camera.far}")
      _nearSlider.notifyChange()
      println(s"modelB: near=${self.model.camera.near}, far=${self.model.camera.far}")
      _farSlider.notifyChange()
      println(s"modelC: near=${self.model.camera.near}, far=${self.model.camera.far}")


  def display: Unit =
    initUI()

  private lazy val _nearSlider = ControlSlider(
    () => self.model.camera.near.toInt,
    value =>
      self.model = self.model.withCamera(self.model.camera.withNear(near = value)), //.notifyChange(),
    label = "near",
    max = self.model.camera.maxNear.toInt,
    min = self.model.camera.minNear.toInt
  )

  private lazy val _farSlider = ControlSlider(
    () => self.model.camera.far.toInt,
    value =>
      self.model = self.model.withCamera(self.model.camera.withFar(far = value)), //.notifyChange(),
    label = "far",
    max = self.model.camera.maxFar.toInt,
    min = self.model.camera.minFar.toInt
  )

  private lazy val _container =

    val panel: JPanel = new JPanel(new GridLayout(1, 0))
    getContentPane().add(panel)

    panel.add(_nearSlider)
    panel.add(_farSlider)
    //    setSize(frameSize)
    setLocationRelativeTo(null) // centered on screen
    //    setResizable(false)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    pack()
    setVisible(true)
    panel

  def initUI(): Unit =
    _container

private[swing] object ControlWindow:

  lazy val _ControlWindow = new ControlWindow()

  def apply(): ControlWindow = _ControlWindow

  def apply(controlModel: ControlModel): ControlWindow =
    _ControlWindow.updateModel(controlModel)
    _ControlWindow

private[swing] class ControlSlider(getter: () => Int,
                                   setter: Int => Unit,
                                   label: String = "",
                                   min: Int = 1,
                                   max: Int = 1000)
  extends JPanel :
  setLayout(new BorderLayout)
  val jLabel = new JLabel(label, SwingConstants.CENTER)
  val initialValue = getter()
  val actualValue =
    if initialValue < min then
      setter(min)
      min
    else
      initialValue
  val actualMax = if actualValue > max then actualValue else max
  println(s"slider($label)=$initialValue, actualValue=$actualValue, actualMax=$actualMax")
  val jSlider = new JSlider(
    SwingConstants.VERTICAL,
    min,
    actualMax,
    actualValue
  )
  add(jLabel, BorderLayout.NORTH)
  add(jSlider, BorderLayout.SOUTH)

  jSlider.addChangeListener(new ChangeListener() {
    override def stateChanged(e: ChangeEvent): Unit = {
      val value = e.getSource.asInstanceOf[JSlider].getValue
      setter(value)
    }
  })

  def notifyChange(): Unit =
    println(s"value = ${getter()}")
    jSlider.setValue(getter())


