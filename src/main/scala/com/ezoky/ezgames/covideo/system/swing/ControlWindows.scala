/*
 * @author gweinbach on 30/06/2022 23:38
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.system.Displays

import java.awt.{BorderLayout, Graphics2D, GraphicsDevice, GraphicsEnvironment, GridLayout, Color as AWTColor, Dimension as AWTDimension, EventQueue as AWTEventQueue}
import javax.swing.*
import javax.swing.event.{ChangeEvent, ChangeListener}

/**
 * @since 0.2.0
 * @author gweinbach on 30/06/2022
 */
trait ControlWindows[I: Identifiable, D: Dimension]
  extends Controls[I, D]
  with Displays[I, D]:

  private class ControlWindow()
    extends JFrame :
  
    self =>
  
    println("Creating a ControlWindow")
  
  
    def display: Unit =
      initUI()
  
    private lazy val _nearSlider = ControlSlider(
      () => Control.model.control(ControlledItem.ViewFrustum).near.toInt,
      value =>
        Control.setModel(Control.model.withControl(ControlledItem.ViewFrustum, Control.model.control(ControlledItem.ViewFrustum).withNear(near = value))),
      label = "near",
      max = Control.model.control(ControlledItem.ViewFrustum).maxNear.toInt,
      min = Control.model.control(ControlledItem.ViewFrustum).minNear.toInt
    )
  
    private lazy val _farSlider = ControlSlider(
      () => Control.model.control(ControlledItem.ViewFrustum).far.toInt,
      value =>
        Control.setModel(Control.model.withControl(ControlledItem.ViewFrustum, Control.model.control(ControlledItem.ViewFrustum).withFar(far = value))),
      label = "far",
      max = Control.model.control(ControlledItem.ViewFrustum).maxFar.toInt,
      min = Control.model.control(ControlledItem.ViewFrustum).minFar.toInt
    )
  
    private lazy val _container =
  
      val panel: JPanel = new JPanel(new GridLayout(1, 0))
      getContentPane().add(panel)
  
      panel.add(_nearSlider)
      panel.add(_farSlider)
      //    setSize(frameSize)
      setLocationRelativeTo(null) // centered on screen
      //    setResizable(false)
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      pack()
      setVisible(true)
      panel
  
    def initUI(): Unit =
      _container
  
  private[swing] object ControlWindow:
  
    lazy val _ControlWindow = new ControlWindow()
  
    def apply(): ControlWindow = _ControlWindow
  
  //  def apply(controlModel: ControlModel): ControlWindow =
  //    _ControlWindow.updateModel(controlModel)
  //    _ControlWindow
  
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
    Control.subscribe(this.notifyChange)
  
    jSlider.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent): Unit = {
        val value = e.getSource.asInstanceOf[JSlider].getValue
        setter(value)
      }
    })
  
    def notifyChange(): Unit =
      println(s"value = ${getter()}")
      jSlider.setValue(getter())
  