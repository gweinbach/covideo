/*
 * @author gweinbach on 30/06/2022 23:38
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.system.Displays
import spire.*
import spire.implicits.*
import spire.math.{*, given}

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{BorderLayout, GridBagConstraints, GridBagLayout, GridLayout}
import javax.swing.*
import javax.swing.event.{ChangeEvent, ChangeListener}

/**
 * @since 0.2.0
 * @author gweinbach on 30/06/2022
 */
trait ControlWindows[I: Identifiable, D: Dimension : Numeric]
  extends SceneControls[I, D]
    with Displays[I, D]:

  import CoordsDimension.{*, given}

  private[swing] class ControlWindow(control: SceneControl)
    extends JFrame:

    self =>

    println("Creating a ControlWindow")
    control.subscribeToUpdates(_nearSlider.notifyChange)
    control.subscribeToUpdates(_farSlider.notifyChange)

    def display(): Unit =
      initUI()

    private lazy val _nearSlider = ControlSlider(
      () => control.getControl(ControlledItem.ViewFrustum).near.toInt,
      value => control.updateControl(ControlledItem.ViewFrustum, _.withNear(near = value.baseValue)),
      label = "near",
      min = control.getControl(ControlledItem.ViewFrustum).minNear.toInt,
      max = control.getControl(ControlledItem.ViewFrustum).maxNear.toInt
    )

    private lazy val _farSlider = ControlSlider(
      () => control.getControl(ControlledItem.ViewFrustum).far.toInt,
      value => control.updateControl(ControlledItem.ViewFrustum, _.withFar(far = value.baseValue)),
      label = "far",
      min = control.getControl(ControlledItem.ViewFrustum).minFar.toInt,
      max = control.getControl(ControlledItem.ViewFrustum).maxFar.toInt
    )

    private lazy val _sliderPanel =
      val sliderPanel: JPanel = new JPanel(new GridLayout(1, 0))
      getContentPane().add(sliderPanel)

      sliderPanel.add(_nearSlider)
      sliderPanel.add(_farSlider)
      sliderPanel

    private lazy val _container =

      val gbLayout = new GridBagLayout()
      val gbConstraints = new GridBagConstraints()

      val panel = getContentPane()
      panel.setLayout(gbLayout)

      val checkbox = new JCheckBox("lock depth", true)
      gbConstraints.fill = GridBagConstraints.HORIZONTAL
      gbConstraints.anchor = GridBagConstraints.PAGE_START
      gbConstraints.gridx = 0
      gbConstraints.gridy = 0
      gbConstraints.gridwidth = 1
      panel.add(checkbox,gbConstraints)

      gbConstraints.anchor = GridBagConstraints.CENTER
      gbConstraints.gridx = 0
      gbConstraints.gridy = 1
      panel.add(_sliderPanel, gbConstraints)

      val exitButton = new JButton("Exit")
      gbConstraints.anchor = GridBagConstraints.SOUTH
      gbConstraints.gridx = 0
      gbConstraints.gridy = 2
      panel.add(exitButton, gbConstraints)

      exitButton.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit =
          control.updateControl(ControlledItem.Game, _.exit())
      })

      //    setSize(frameSize)
//      setLocationRelativeTo(null) // centered on screen
      //    setResizable(false)
      setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
      pack()
      setVisible(true)
      panel

    def initUI(): Unit =
      _container

  private[swing] object ControlWindow:

    lazy val _ControlWindow = new ControlWindow(SceneControl)

    def apply(): ControlWindow = _ControlWindow


  private[swing] class ControlSlider(getter: () => Int,
                                     setter: Int => Unit,
                                     label: String = "",
                                     min: Int = 1,
                                     max: Int = 1000)
    extends JPanel:
    setLayout(new BorderLayout)
    val jLabel = new JLabel(label, SwingConstants.CENTER)
    val initialValue = getter()
    val actualValue =
      if initialValue < min then
        setter(min)
        min
      else
        initialValue
    val actualMax =
      if actualValue > max then actualValue else max
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
//      println(s"value = ${getter()}")
      jSlider.setValue(getter())
