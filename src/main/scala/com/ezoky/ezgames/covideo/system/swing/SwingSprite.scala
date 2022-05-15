package com.ezoky.ezgames.covideo.system.swing

import com.ezoky.ezgames.covideo.component.{Position, Sprite}

import java.awt.image.BufferedImage as AWTImage


case class SwingSprite(asset: AWTImage,
                       position: Position,
                       previousPosition: Option[Position] = None)
  extends Sprite :

  override type ImageType = AWTImage

  override val image: ImageType = asset

  override def moveTo(position: Position): SwingSprite =
    copy(
      position = position,
      previousPosition = Some(this.position)
    )
