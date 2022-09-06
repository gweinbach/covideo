/*
 * @author gweinbach on 30/06/2022 23:29
 * @since 0.2.0
 */

package com.ezoky.ezgames.covideo.system.swing


import com.ezoky.ez3d.Screen.*
import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.*
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.HealthCondition.*
import com.ezoky.ezgames.covideo.component.swing.SwingSprite
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}
import com.ezoky.ezgames.covideo.entity.{*, given}
import com.ezoky.ezgames.covideo.system.{ControlledItem, DisplaySystem}

import java.awt.event.{KeyEvent, KeyListener}
import java.awt.image.BufferedImage as AWTImage
import java.awt.{Graphics2D, GraphicsDevice, GraphicsEnvironment, Color as AWTColor, Dimension as AWTDimension, EventQueue as AWTEventQueue}
import javax.swing.border.Border
import javax.swing.{BorderFactory, JFrame, JPanel}
import scala.collection.mutable.Map as MutableMap

/**
 * @since 0.2.0
 * @author gweinbach on 30/06/2022
 */

private given Conversion[Pixel, Int] with
  def apply(pixel: Pixel): Int = pixel.asInt

extension (sceneDimension: ScreenDimension)
  private def awtDimension: AWTDimension =
    AWTDimension(sceneDimension.width, sceneDimension.height)

extension (awtDimension: AWTDimension)
  private def isNull: Boolean =
    (awtDimension.height == 0) &&
      (awtDimension.width == 0)

private class SceneWindow()
  extends JFrame :

  println("Creating a SceneWindow")

  private val panel: DrawingPanel = new DrawingPanel()
  private var panelSize: AWTDimension = new AWTDimension()

  display()

  def resizeScene(size: ScreenDimension): Unit =
    if (size.awtDimension != panelSize)
      panel.setPreferredSize(
        size.awtDimension
      )
      panelSize = size.awtDimension
      pack()
  //      repaint()

  def updateTitle(newTitle: String): Unit =
    if (newTitle != getTitle)
      setTitle(newTitle)

  def draw(scene: Scene): Unit =
    panel.updateScene(scene)

  private def display(): Unit =
    if !panelSize.isNull then
      panel.setPreferredSize(panelSize)

    panel.setFocusable(true)
    panel.addKeyListener(new KeyHandler)
    add(panel)

    setLocationRelativeTo(null) // centered on screen
    //    setResizable(false)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    pack()
    setVisible(true)


private[swing] object SceneWindow:

  // singleton
  private val _SceneWindows: MutableMap[SceneId, SceneWindow] = MutableMap.empty

  def apply(sceneId: SceneId): SceneWindow =
    _SceneWindows.getOrElse(sceneId, {
      val mainWindow = new SceneWindow()
      _SceneWindows.addOne(sceneId, mainWindow)
      mainWindow
    })

/**
 * Drawing Panel
 */
private class DrawingPanel()
  extends JPanel :

  println("Creating a DrawingPanel")
  private var optScene: Option[Scene] = None

  import java.awt.{Graphics, Graphics2D, Toolkit}

  setDoubleBuffered(true)
  setBackground(AWTColor.black)

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    doDrawing(g)
    Toolkit.getDefaultToolkit.sync()

  private def doDrawing(g: Graphics): Unit =
    val g2d = g.asInstanceOf[Graphics2D]
    for
      scene <- optScene
    yield
      val pipeline3D = new Pipeline3D(scene.camera, scene)
      g2d.setColor(AWTColor.white)
      for
        component <- scene.components
      yield
        val screenShape = pipeline3D.run(component)
        for
          segment <- screenShape.segments
        yield
          //          println(s"segment=$segment")
          g2d.drawLine(
            segment.s.x,
            segment.s.y,
            segment.t.x,
            segment.t.y,
          )

    //      for
    //        sprite <- scene.sprites.values
    //      yield
    //        val awtImage = sprite.image.asInstanceOf[AWTImage]

    //            for
    //              previousPosition <- sprite.previousPosition
    //            yield
    //              val previousScenePosition = scene.project(previousPosition)
    //              g2d.clearRect(
    //                previousScenePosition.x,
    //                previousScenePosition.y,
    ////                awtImage.getWidth(this),
    ////                awtImage.getHeight(this)
    //                20,20
    //              )

    //        val spritePosition = scene.project(sprite.position)
    //        g2d.drawImage(awtImage, spritePosition.x, spritePosition.y, this)

    g2d.dispose()


  def updateScene(scene: Scene): Unit =
    if (optScene.isEmpty)
      setPreferredSize(scene.preferredDimension.awtDimension)
      val border: Border = BorderFactory.createMatteBorder(
        scene.margins.top,
        scene.margins.left,
        scene.margins.bottom,
        scene.margins.right,
        AWTColor.black
      )
      setBorder(border)

    optScene = Some(scene)
    repaint()


class KeyHandler extends KeyListener :

  def keyTyped(e: KeyEvent): Unit = {
//    displayInfo(e, "KEY TYPED: ")
  }

  def keyPressed(e: KeyEvent): Unit = {
//    displayInfo(e, "KEY PRESSED: ")
    e.getKeyCode() match
      case KeyEvent.VK_UP =>
        println("UP")
        Control.setModel(Control.model.withControl(ControlledItem.Camera, Control.model.control(ControlledItem.Camera).plusDy(100)))
      case KeyEvent.VK_DOWN =>
        println("DOWN")
        Control.setModel(Control.model.withControl(ControlledItem.Camera, Control.model.control(ControlledItem.Camera).plusDy(-100)))
      case KeyEvent.VK_LEFT =>
        println("LEFT")
        Control.setModel(Control.model.withControl(ControlledItem.Camera, Control.model.control(ControlledItem.Camera).plusDx(-100)))
      case KeyEvent.VK_RIGHT =>
        println("RIGHT")
        Control.setModel(Control.model.withControl(ControlledItem.Camera, Control.model.control(ControlledItem.Camera).plusDx(100)))
      case _ =>
        ()
  }

  def keyReleased(e: KeyEvent): Unit = {
//    displayInfo(e, "KEY RELEASED: ")
  }

  private def displayInfo(e: KeyEvent,
                          keyStatus: String): Unit = {

    //You should only rely on the key char if the event
    //is a key typed event.
    val id = e.getID()
    var keyString = ""
    if (id == KeyEvent.KEY_TYPED) {
      val c = e.getKeyChar();
      keyString = "key character = '" + c + "'";
    } else {
      val keyCode = e.getKeyCode();
      keyString = "key code = " + keyCode
        + " ("
        + KeyEvent.getKeyText(keyCode)
        + ")"
    }

    val modifiersEx = e.getModifiersEx()
    var modString = "extended modifiers = " + modifiersEx
    var tmpString = KeyEvent.getKeyModifiersText(modifiersEx)

    if (tmpString.length() > 0) {
      modString += " (" + tmpString + ")"
    } else {
      modString += " (no extended modifiers)"
    }

    var actionString = "action key? "
    if (e.isActionKey()) {
      actionString += "YES"
    } else {
      actionString += "NO"
    }

    var locationString = "key location: "
    val location = e.getKeyLocation()
    if (location == KeyEvent.KEY_LOCATION_STANDARD) {
      locationString += "standard"
    } else if (location == KeyEvent.KEY_LOCATION_LEFT) {
      locationString += "left"
    } else if (location == KeyEvent.KEY_LOCATION_RIGHT) {
      locationString += "right"
    } else if (location == KeyEvent.KEY_LOCATION_NUMPAD) {
      locationString += "numpad"
    } else { // (location == KeyEvent.KEY_LOCATION_UNKNOWN)
      locationString += "unknown"
    }

    //Display information about the KeyEvent...
    println(s"keyStatus = $keyStatus")
    println(s"keyString = $keyString")
    println(s"modString = $modString")
    println(s"actionString = $actionString")
    println(s"locationString = $locationString")
  }
