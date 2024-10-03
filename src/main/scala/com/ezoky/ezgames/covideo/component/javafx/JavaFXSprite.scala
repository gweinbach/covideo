package com.ezoky.ezgames.covideo.component.javafx

import com.ezoky.ezgames.covideo.assets.Asset
import com.ezoky.ezgames.covideo.component.{Dimension, Sprites}
import javafx.scene.image.Image as JavaFXImage

trait JavaFXSprites[D: Dimension]
  extends Sprites[D]:

  import CoordsDimension.*

  case class JavaFXSprite private(image: JavaFXImage,
                                  definition: SpriteDefinition,
                                  position: Position = Position.Zero,
                                  previousPosition: Option[Position] = None)
    extends Sprite:

    override type ImageType = JavaFXImage

    override def moveTo(position: Position): JavaFXSprite =
      copy(
        position = position,
        previousPosition = Some(this.position)
      )


  object JavaFXSprite:

    def apply(origin: Sprite): JavaFXSprite =
      origin match
        case javaFXSprite: JavaFXSprite =>
          javaFXSprite
        case _ =>
          new JavaFXSprite(
            new JavaFXImage(origin.definition.image.url.toString),
            origin.definition,
            origin.position,
            origin.previousPosition
          )

//    def getNonTransparentArea(image: JavaFXImage): Area =
//      val area = new Area
//      for (x <- 0 until image.getWidth)
//        for (y <- 0 until image.getHeight)
//          val pixel = image.getRGB(x, y)
//          if (isTransparent(pixel))
//            val r = new Rectangle(x, y, 1, 1)
//            area.add(new Area(r))
//      area
//
//    private def isTransparent(pixel: Int): Boolean =
//      (pixel & 0xff000000) != 0

