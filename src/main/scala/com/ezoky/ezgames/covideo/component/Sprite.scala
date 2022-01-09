package com.ezoky.ezgames.covideo.component

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Sprite[ImageType] {

  val image: ImageType

  val position: Position

  def moveTo(position: Position): Sprite[ImageType] 
  
}
