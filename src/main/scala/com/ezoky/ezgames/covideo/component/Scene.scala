package com.ezoky.ezgames.covideo.component

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Scene[ImageType] {

  def project(position: Position): (Int, Int)

  def withSprite(sprite: Sprite[ImageType]): Scene[ImageType]

  def withArea(area: Area): Scene[ImageType]
}

