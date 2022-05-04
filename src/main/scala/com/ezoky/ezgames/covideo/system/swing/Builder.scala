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


opaque type Pixel = Int

extension (pixel: Pixel)
  def px: Pixel = pixel

  def +(other: Pixel): Pixel =
    pixel + other

  def -(other: Pixel): Pixel =
    pixel - other

  def width(using Geometry: Geometry): Width =
    Width(pixel size)

  def height(using Geometry: Geometry): Height =
    Height(pixel size)


case class CurrentScreenAreaBuilder(geometry: Geometry,
                                    topMargin: Pixel,
                                    leftMargin: Pixel,
                                    bottomMargin: Pixel,
                                    rightMargin: Pixel)
  extends Builder[Area] :

  import java.awt.{GraphicsDevice, GraphicsEnvironment}

  val gd: GraphicsDevice = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
  val screenWidth: Pixel = gd.getDisplayMode.getWidth px
  val screenHeight: Pixel = gd.getDisplayMode.getHeight px

  given Geometry = geometry

  override def build: Generated[Area] =
    Generated.unit(
      Area(
        (screenWidth - leftMargin - rightMargin).width,
        (screenHeight - topMargin - bottomMargin).height,
        Depth.Flat
      )
    )


case class CurrentScreenSceneBuilder(area: Area)
  extends Builder[Scene] :

  override def build: Generated[Scene] =
    val panel = SwingScene().withArea(area)
    Generated.unit(panel)


case class SwingSprite(asset: ImageIcon,
                       position: Position)
  extends Sprite :

  override type ImageType = AWTImage

  override val image: ImageType = asset.getImage

  override def moveTo(position: Position): SwingSprite =
    copy(position = position)


case class SwingScene(area: Option[Area] = None,
                      sprites: Set[Sprite] = Set.empty,
                      displayFrame: DisplayFrame = DisplayFrame())
  extends Scene {

  EventQueue.invokeLater(() =>
    displayFrame.setVisible(true)
  )

  override def withArea(area: Area): SwingScene = {
    val withArea = copy(area = Some(area))
    // side effect, not pure
    displayFrame.resize(project(area.maxPosition))
    withArea
  }

  override def withSprite(sprite: Sprite): SwingScene = {
    val withSprite = copy(sprites = sprites + sprite)
    // side effect, not pure
    displayFrame.updateScene(withSprite)
    withSprite
  }

  override def project(position: Position): (Int, Int) =
    (position.x.value.intValue, position.y.value.intValue)
}


private class DisplayFrame(var frameSize: (Int, Int) = (0, 0),
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

  def resize(size: (Int, Int)): Unit = {
    setSize(
      size._1,
      size._2
    )
    frameSize = size
    repaint()
  }

  def updateScene(scene: SwingScene): Unit = {
    panel = panel.updateScene(scene)
    repaint()
  }
}

private class DisplayPanel(var optScene: Option[SwingScene] = None)
  extends JPanel :

  import java.awt.{Color, Graphics, Graphics2D, Toolkit}

  //  setBackground(Color.black);

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    doDrawing(g)
    Toolkit.getDefaultToolkit.sync()


  private def doDrawing(g: Graphics): Unit =
    val g2d = g.asInstanceOf[Graphics2D]
    for
      scene <- optScene.toSet
      sprite <- scene.sprites
    yield
      val (positionX, positionY) = scene.project(sprite.position)
      g2d.drawImage(sprite.image.asInstanceOf[AWTImage], positionX, positionY, this)



  def updateScene(scene: SwingScene): DisplayPanel =
    optScene = Some(scene)
    this


case class WorldBuilder(worldConfig: WorldConfig)
  extends Builder[World] :

  override def build: Generated[World] =

    for
      area <- CurrentScreenAreaBuilder(
        worldConfig.geometry,
        topMargin = worldConfig.topMargin,
        leftMargin = worldConfig.leftMargin,
        bottomMargin = worldConfig.bottomMargin,
        rightMargin = worldConfig.rightMargin
      ).build
      scene <- CurrentScreenSceneBuilder(area).build
    yield
      World(
        area,
        scene
      )


case class PersonBuilder(area: Area,
                         personConfig: PersonConfig)
  extends Builder[Person] :

  override def build: Generated[Person] =
    for
      position <- Position.generated(area)
      speed <- Speed.generated(personConfig.speedRange, personConfig.speedRange, personConfig.speedRange)
    yield Person(
      position,
      speed,
      Healthy,
      SwingSprite(Assets.SmileySunglasses, position)
    )


case class GameBuilder(gameConfig: GameConfig)
  extends Builder[Game] :

  override def build: Generated[Game] =
    for
      world <- WorldBuilder(gameConfig.worldConfig).build
      population <- Generated.setOf(PersonBuilder(world.area, gameConfig.personConfig).build)(gameConfig.populationSize)
    yield
      Game(
        world,
        population
      )



case class WorldConfig(geometry: Geometry,
                       topMargin: Pixel,
                       leftMargin: Pixel,
                       bottomMargin: Pixel,
                       rightMargin: Pixel)

case class PersonConfig(speedRange: SpeedRange)

case class GameConfig(populationSize: Int,
                      personConfig: PersonConfig,
                      worldConfig: WorldConfig)


//
//case class ImageBuilder() extends Builder[Sprite[ImageType]] {
//  override def build: Sprite[ImageType] = ???
//}
