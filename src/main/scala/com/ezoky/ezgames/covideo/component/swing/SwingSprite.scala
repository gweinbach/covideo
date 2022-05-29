
package com.ezoky.ezgames.covideo.component.swing

import com.ezoky.ezgames.covideo.component.{Position, Sprite}

import java.awt.image.BufferedImage as AWTImage
import javax.imageio.ImageIO

/**
 * @author gweinbach on 29/05/2022
 * @since 0.2.0
 */
case class SwingSprite(asset: AWTImage,
                       position: Position = Position.Zero,
                       previousPosition: Option[Position] = None)
  extends Sprite:

  override type ImageType = AWTImage

  override val image: ImageType = asset
  
  override def moveTo(position: Position): SwingSprite =
    copy(
      position = position,
      previousPosition = Some(this.position)
    )

object SwingSprite:

  val SmileySunglasses = SwingSprite(Assets.SmileySunglasses)
  val SmileySick = SwingSprite(Assets.SmileySick)


private object Assets:

  val SmileySunglasses = ImageIO.read(getClass().getResource("smiley-sunglasses-33x33.png"))
  val SmileySick = ImageIO.read(getClass().getResource("sick-emoji-33x33.png"))
