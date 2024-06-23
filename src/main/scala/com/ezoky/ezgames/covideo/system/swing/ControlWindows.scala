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

import java.awt.event.{ActionEvent, ItemEvent, WindowAdapter, WindowEvent}
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
    control.subscribeToUpdates(_populationSizeValue.notifyChange)

    def display(): Unit =
      initUI()

    /**
     * Used to control View Frustum's near pane distance from Camera
     */
    private lazy val _nearSlider = ControlSlider(
      () => control.getControl(ControlledItem.ViewFrustum).near.toInt,
      value => control.updateControl(ControlledItem.ViewFrustum, _.withNear(near = value.baseValue)),
      label = "near",
      min = control.getControl(ControlledItem.ViewFrustum).minNear.toInt,
      max = control.getControl(ControlledItem.ViewFrustum).maxNear.toInt
    )

    /**
     * Used to control View Frustum's far pane distance from Camera
     */
    private lazy val _farSlider = ControlSlider(
      () => control.getControl(ControlledItem.ViewFrustum).far.toInt,
      value => control.updateControl(ControlledItem.ViewFrustum, _.withFar(far = value.baseValue)),
      label = "far",
      min = control.getControl(ControlledItem.ViewFrustum).minFar.toInt,
      max = control.getControl(ControlledItem.ViewFrustum).maxFar.toInt
    )

    /**
     * Used to disply population size
     */
    private lazy val _populationSizeValue = ValueDisplay(
      () => control.getControl(ControlledItem.Game).populationSize.toString
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

      gbConstraints.fill = GridBagConstraints.HORIZONTAL
      gbConstraints.anchor = GridBagConstraints.PAGE_START
      gbConstraints.gridx = 0
      gbConstraints.gridy = 0
      gbConstraints.gridwidth = 1
      panel.add(_populationSizeValue, gbConstraints)

      val checkbox = new JCheckBox("lock depth", true)
      gbConstraints.fill = GridBagConstraints.HORIZONTAL
      gbConstraints.anchor = GridBagConstraints.PAGE_START
      gbConstraints.gridx = 0
      gbConstraints.gridy = 1
      gbConstraints.gridwidth = 1
      panel.add(checkbox, gbConstraints)
      checkbox.addItemListener((e: ItemEvent) =>
        val lockDepth = (e.getStateChange() == ItemEvent.SELECTED)
        control.updateControl(ControlledItem.ViewFrustum, _.withLockDepth(lockDepth))
      )

      gbConstraints.anchor = GridBagConstraints.CENTER
      gbConstraints.gridx = 0
      gbConstraints.gridy = 2
      panel.add(_sliderPanel, gbConstraints)

      val exitButton = new JButton("Exit")
      gbConstraints.anchor = GridBagConstraints.PAGE_END
      gbConstraints.gridx = 0
      gbConstraints.gridy = 3
      panel.add(exitButton, gbConstraints)


      exitButton.addActionListener((e: ActionEvent) =>
        control.updateControl(ControlledItem.Game, _.exit())
      )

      setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
      addWindowListener(new WindowAdapter() {
        override def windowClosing(e: WindowEvent): Unit = {
          control.updateControl(ControlledItem.Game, _.exit())
        }
      })

      //    setSize(frameSize)
      //    setResizable(false)
      setLocationRelativeTo(null) // centered on screen

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
                                     label: String,
                                     min: Int,
                                     max: Int)
    extends JPanel:

    setLayout(new BorderLayout)
    private val _jLabel = new JLabel(label, SwingConstants.CENTER)
    private val _initialValue = getter()
    private val _actualValue =
      if _initialValue < min then
        setter(min)
        min
      else
        _initialValue
    private val _actualMax =
      if _actualValue > max then _actualValue else max
    println(s"slider($label)=$_initialValue, actualValue=$_actualValue, actualMax=$_actualMax")
    private val _jSlider = new JSlider(
      SwingConstants.VERTICAL,
      min,
      _actualMax,
      _actualValue
    )
    add(_jLabel, BorderLayout.NORTH)
    add(_jSlider, BorderLayout.SOUTH)

    _jSlider.addChangeListener(new ChangeListener() {
      override def stateChanged(e: ChangeEvent): Unit = {
        val value = e.getSource.asInstanceOf[JSlider].getValue
        setter(value)
      }
    })

    def notifyChange(): Unit =
//      println(s"slider value = ${getter()}")
      _jSlider.setValue(getter())

  
  private[swing] class ValueDisplay(getter: () => String)
    extends JLabel:

    setText(getter())

    def notifyChange(): Unit =
//      println(s"displayed value = ${getter()}")
      setText(getter())