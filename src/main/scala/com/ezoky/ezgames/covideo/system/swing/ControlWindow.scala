/*
 * @author gweinbach on 30/06/2022 23:38
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.system.{ControlModel, CameraControl}
import com.ezoky.ezgames.covideo.component.Dimension.DimensionBase

import java.awt.{Graphics2D, GraphicsDevice, GraphicsEnvironment, Color as AWTColor, Dimension as AWTDimension, EventQueue as AWTEventQueue}
import javax.swing.{JFrame, JSlider, JPanel, SwingConstants}
import javax.swing.event.{ChangeEvent, ChangeListener}

/**
 * @since 0.2.0
 * @author gweinbach on 30/06/2022
 */
private class ControlWindow(var model: ControlModel = ControlModel(CameraControl(50, 150)))
  extends JFrame:

  self =>

  println("Creating a ControlWindow")

  def display: Unit =
    initUI()

  private lazy val _nearSlider: JSlider =
    val slider = new JSlider(
      SwingConstants.VERTICAL,
      1,
      1000,
      50
    )

    slider.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent): Unit = {
        println(s"Setting slider near value = ${e.getSource.asInstanceOf[JSlider].getValue}")
        self.model = self.model.copy(camera = self.model.camera.copy(nearValue = e.getSource.asInstanceOf[JSlider].getValue))
      }
    })
    slider

  private lazy val _farSlider: JSlider =
    val slider = new JSlider(
      SwingConstants.VERTICAL,
      1,
      1000,
      150
    )

    slider.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent): Unit = {
        println(s"Setting slider far value = ${e.getSource.asInstanceOf[JSlider].getValue}")
        self.model = self.model.copy(camera = self.model.camera.copy(farValue = e.getSource.asInstanceOf[JSlider].getValue))
      }
    })
    slider


  private lazy val _container =

    val panel: JPanel = new JPanel
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

private [swing] object ControlWindow:

  lazy val _ControlWindow = new ControlWindow()

  def apply(): ControlWindow = _ControlWindow

