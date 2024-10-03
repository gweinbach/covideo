
package com.ezoky.ezgames.covideo.component.swing

import com.ezoky.ezgames.covideo.assets.Asset
import com.ezoky.ezgames.covideo.component.{Dimension, Sprites}

import java.awt.Rectangle
import java.awt.geom.Area
import java.awt.image.BufferedImage as AWTImage
import javax.imageio.ImageIO

/**
 * @author gweinbach on 29/05/2022
 * @since 0.2.0
 */
trait SwingSprites[D: Dimension]
  extends Sprites[D]:
  
  import CoordsDimension.*
  
  case class SwingSprite(image: AWTImage,
                         definition: SpriteDefinition,
                         position: Position = Position.Zero,
                         previousPosition: Option[Position] = None)
    extends Sprite:
  
    override type ImageType = AWTImage
    
    override def moveTo(position: Position): SwingSprite =
      copy(
        position = position,
        previousPosition = Some(this.position)
      )
  
  object SwingSprite:

    def apply(origin: Sprite): SwingSprite =
      origin match
        case swingSprite: SwingSprite =>
          swingSprite
        case _ =>
          new SwingSprite(
            ImageIO.read(origin.definition.image.url),
            origin.definition,
            origin.position,
            origin.previousPosition
          )
  
    def getNonTransparentArea(image: AWTImage): Area =
      val area = new Area
      for (x <- 0 until image.getWidth)
        for (y <- 0 until image.getHeight)
          val pixel = image.getRGB(x, y)
          if (isTransparent(pixel))
            val r = new Rectangle(x, y, 1, 1)
            area.add(new Area(r))
      area
  
    private def isTransparent(pixel: Int): Boolean =
      (pixel & 0xff000000) != 0
