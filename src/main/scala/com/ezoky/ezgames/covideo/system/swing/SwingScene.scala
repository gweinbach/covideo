package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.{Area, Sprite, Position}
import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.entity.*
import com.ezoky.ezgames.covideo.system.Builder

import java.awt.image.BufferedImage as AWTImage
import java.awt.{Graphics2D, Color as AWTColor, Dimension as AWTDimension, EventQueue as AWTEventQueue}
import javax.swing.border.Border
import javax.swing.{BorderFactory, JFrame, JPanel}

/**
 * @author gweinbach on 14/05/2022 12:29
 * @since 0.2.0
 */
case class SwingScene(optArea: Option[Area] = None,
                      margins: Margin = Margin(),
                      sprites: Population[Sprite] = Population.empty,
                      mainWindow: MainWindow = MainWindow())
  extends Scene :

  AWTEventQueue.invokeLater(() =>
    // side effect, not pure
    mainWindow.setVisible(true)
    mainWindow.resize(preferredDimension)
  )

  override lazy val preferredDimension: SceneDimension =
    optArea.fold(SceneDimension(0 px, 0 px))(area => project(area) withMargin margins)

  override def project(position: Position): ScenePosition =
    ScenePosition(
      margins.left + position.x.value.intValue px,
      margins.top + position.y.value.intValue px
    )

  override def project(area: Area): SceneDimension =
    SceneDimension(
      area.maxPosition.x.value.intValue px,
      area.maxPosition.y.value.intValue px,
    )

  override def withSprite(id: PersonId,
                          sprite: Sprite): SwingScene =
    val withSprite = copy(sprites = sprites.add(id -> sprite))
    // side effect, not pure
    mainWindow.draw(withSprite)
    withSprite

  override def withArea(area: Area): SwingScene =
    copy(optArea = Some(area))


private case class SwingSceneBuilder(sceneConfig: SceneConfig,
                                     area: Area)
  extends Builder[Scene] :

  override def build: Generated[Scene] =
    val frame = MainWindow(name = sceneConfig.name)
    val scene = SwingScene(margins = sceneConfig.margin, mainWindow = frame).withArea(area)
    Generated.unit(scene)


//type Drawn[A] = (A, Graphics2D) => (A, Graphics2D)
//
//case class Draw[A: Drawn](toDraw: A)
//
//given Drawn[Sprite] with
//  in =>
//    val (sprite: SwingSprite, g2d: Graphics2D) = in
//    val awtImage = sprite.image.asInstanceOf[AWTImage]
//    val spritePosition = scene.project(sprite.position)
//    g2d.drawImage(awtImage, spritePosition.x, spritePosition.y, this)
//    (sprite, g2d)
//

private class MainWindow(var name: String = "",
                         var frameSize: AWTDimension = new AWTDimension(),
                         val panel: DrawingPanel = new DrawingPanel())
  extends JFrame:

  println("Creating a MainWindow")

  initUI()

  private def initUI(): Unit =
    getContentPane().add(panel)
    setTitle(name)
    setSize(frameSize)
    setLocationRelativeTo(null) // centered on screen
    setResizable(false)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setVisible(true)

  def resize(size: SceneDimension): Unit =
    setSize(
      size.awtDimension
    )
    frameSize = size.awtDimension
    repaint()

  def draw(scene: SwingScene): Unit =
    panel.updateScene(scene)
    (scene, this)


/**
 * Drawing Panel
 */
private class DrawingPanel()
  extends JPanel :

  println("Creating a DrawingPanel")
  private var optScene: Option[SwingScene] = None

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


  def updateScene(scene: SwingScene): Unit =
    if (optScene.isEmpty)
      setPreferredSize(scene.preferredDimension.awtDimension)
      //    setLocation(scene.margins.left, scene.margins.top)
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
