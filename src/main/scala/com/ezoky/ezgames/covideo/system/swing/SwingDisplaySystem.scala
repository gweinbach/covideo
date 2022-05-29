package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.HealthCondition.*
import com.ezoky.ezgames.covideo.component.Screen.*
import com.ezoky.ezgames.covideo.component.swing.SwingSprite
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}
import com.ezoky.ezgames.covideo.system.DisplaySystem

import java.awt.image.BufferedImage as AWTImage
import java.awt.{Graphics2D, GraphicsDevice, GraphicsEnvironment, Color as AWTColor, Dimension as AWTDimension, EventQueue as AWTEventQueue}
import javax.swing.border.Border
import javax.swing.{BorderFactory, JFrame, JPanel}

/**
 * @author gweinbach on 14/05/2022 12:29
 * @since 0.2.0
 */
object SwingDisplaySystem
  extends DisplaySystem :

  override def defaultScreenSceneDimension: ScreenDimension =
    import java.awt.{GraphicsDevice, GraphicsEnvironment}

    val gd: GraphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
    val screenWidth: Pixel = gd.getDisplayMode.getWidth px
    val screenHeight: Pixel = gd.getDisplayMode.getHeight px

    ScreenDimension(screenWidth, screenHeight)

  override def drawScene(scene: Scene): IO[Unit] =
    IO {
      // side effects, not pure
      val mainWindow = MainWindow(scene.id)
      mainWindow.updateTitle(scene.name)
      mainWindow.resize(scene.preferredDimension)
      mainWindow.draw(scene)
    }

  override def spriteByHealthCondition(healthCondition: HealthCondition): Sprite =
    healthCondition match
      case Healthy => SwingSprite.SmileySunglasses
      case Sick => SwingSprite.SmileySick


private given Conversion[Pixel, Int] with
  def apply(pixel: Pixel): Int = pixel.asInt

extension (sceneDimension: ScreenDimension)
  private def awtDimension: AWTDimension =
    AWTDimension(sceneDimension.width, sceneDimension.height)

private class MainWindow()
  extends JFrame :

  println("Creating a MainWindow")

  private var frameSize: AWTDimension = new AWTDimension()

  private val panel: DrawingPanel = new DrawingPanel()

  initUI()

  private def initUI(): Unit =
    getContentPane().add(panel)
    setSize(frameSize)
    setLocationRelativeTo(null) // centered on screen
    setResizable(false)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setVisible(true)

  def resize(size: ScreenDimension): Unit =
    if (size.awtDimension != frameSize)
      setSize(
        size.awtDimension
      )
      frameSize = size.awtDimension
      repaint()


  def updateTitle(newTitle: String): Unit =
    if (newTitle != getTitle)
      setTitle(newTitle)

  def draw(scene: Scene): Unit =
    panel.updateScene(scene)

private object MainWindow:

  // singleton
  private val _MainWindows: scala.collection.mutable.Map[SceneId, MainWindow] = scala.collection.mutable.Map.empty

  def apply(sceneId: SceneId): MainWindow =
    _MainWindows.getOrElse(sceneId, {
      val mainWindow = new MainWindow()
      _MainWindows.addOne(sceneId, mainWindow)
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
      scene <- optScene.toIterable
      sprite <- scene.sprites.values
    yield
      val awtImage = sprite.image.asInstanceOf[AWTImage]

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

      val spritePosition = scene.project(sprite.position)
      g2d.drawImage(awtImage, spritePosition.x, spritePosition.y, this)

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
