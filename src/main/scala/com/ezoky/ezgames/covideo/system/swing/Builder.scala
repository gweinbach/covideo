package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Dimension.*
import com.ezoky.ezgames.covideo.entity.{Game, Person, World}
import com.ezoky.ezgames.covideo.system.Builder

import java.awt.{EventQueue, Image as AWTImage}
import javax.swing.{ImageIcon, JFrame, JPanel}

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */


type ImageType = AWTImage

opaque type Pixel = Int

extension (pixel: Pixel) {
  def px: Pixel = pixel

  def +(other: Pixel): Pixel =
    pixel + other

  def -(other: Pixel): Pixel =
    pixel - other

  def width(using Geometry: Geometry): Width =
    Width(pixel size)

  def height(using Geometry: Geometry): Height =
    Height(pixel size)

}

case class CurrentScreenAreaBuilder(topMargin: Pixel,
                                    leftMargin: Pixel,
                                    bottomMargin: Pixel,
                                    rightMargin: Pixel)(using Geometry)
  extends Builder[Area] {

  import java.awt.{GraphicsDevice, GraphicsEnvironment}

  val gd: GraphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
  val screenWidth: Pixel = gd.getDisplayMode.getWidth px
  val screenHeight: Pixel = gd.getDisplayMode.getHeight px

  override def build: Area = Area(
    (screenWidth - leftMargin - rightMargin).width,
    (screenHeight - topMargin - bottomMargin).height,
    Depth.Flat
  )
}

case class CurrentScreenSceneBuilder(area: Area)
  extends Builder[Scene[ImageType]] {
  override def build: Scene[ImageType] = {
    val panel = SwingScene().withArea(area)
    panel
  }
}

case class SwingSprite(asset: ImageIcon,
                       position: Position) extends Sprite[ImageType] {

  val image: ImageType = asset.getImage

  def moveTo(position: Position): SwingSprite =
    copy(position = position)

}


case class SwingScene(area: Option[Area] = None,
                      sprites: Set[Sprite[ImageType]] = Set.empty,
                      var displayFrame: DisplayFrame = DisplayFrame())
  extends Scene[ImageType] {

  EventQueue.invokeLater(() =>
    displayFrame.setVisible(true)
  )

  override def withArea(area: Area): SwingScene = {
    val withArea = copy(area = Some(area))
    withArea.displayFrame = displayFrame.resize(project(area.maxPosition))
    withArea
  }

  override def withSprite(sprite: Sprite[ImageType]): SwingScene = {
    val withSprite = copy(sprites = sprites + sprite)
    withSprite.displayFrame = displayFrame.updateScene(withSprite)
    withSprite
  }

  override def project(position: Position): (Int, Int) =
    (position.x.value.intValue, position.y.value.intValue)
}


class DisplayFrame(var frameSize: (Int, Int) = (0,0),
                   var panel: DisplayPanel = new DisplayPanel())
  extends JFrame {

  initUI()

  private def initUI(): Unit = {
    add(panel)
    setTitle("COVIDEO")
    setSize(frameSize._1, frameSize._2)
    setLocationRelativeTo(null)
    setResizable(false)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }

  def resize(size: (Int, Int)): DisplayFrame = {
    setSize(
      size._1,
      size._2
    )
    frameSize = size
    repaint()
    this
  }

  def updateScene(scene: SwingScene): DisplayFrame = {
    panel = panel.updateScene(scene)
    repaint()
    this
  }
}

class DisplayPanel(var optScene: Option[SwingScene] = None)
  extends JPanel {

  import java.awt.{Color, Graphics, Graphics2D, Toolkit}

  //  setBackground(Color.black);

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    doDrawing(g)
    Toolkit.getDefaultToolkit.sync()
  }

  private def doDrawing(g: Graphics): Unit = {
    val g2d = g.asInstanceOf[Graphics2D]
    for {
      scene <- optScene.toSet
      sprite <- scene.sprites
    } yield {
      val (positionX, positionY) = scene.project(sprite.position)
      g2d.drawImage(sprite.image, positionX, positionY, this)
    }
  }

  def updateScene(scene: SwingScene): DisplayPanel = {
    optScene = Some(scene)
    this
  }
}

case class WorldBuilder()(using Geometry) extends Builder[World[ImageType]] {
  override def build: World[ImageType] = {
    val area = CurrentScreenAreaBuilder(50 px, 50 px, 50 px, 50 px).build
    World(
      area,
      CurrentScreenSceneBuilder(area).build
    )
  }
}

case class PersonBuilder(area: Area)(using Geometry) extends Builder[Person[ImageType]] {
  override def build: Person[ImageType] = {

    val position = area.randomPosition
    Person(
      position,
      Movement.Zero,
      Healthy,
      SwingSprite(Assets.SmileySunglasses, position)
    )
  }
}

case class GameBuilder()(using Geometry) extends Builder[Game[ImageType]] {
  override def build: Game[ImageType] = {
    val world = WorldBuilder().build
    Game(
      world,
      Set.fill(50)(PersonBuilder(world.area).build)
    )
  }
}

//
//case class ImageBuilder() extends Builder[Sprite[ImageType]] {
//  override def build: Sprite[ImageType] = ???
//}
