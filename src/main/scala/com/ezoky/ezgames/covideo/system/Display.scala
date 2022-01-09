package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Scene, Sprite}

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
case class Display[ImageType](scene: Scene[ImageType],
                              sprites: Set[Sprite[ImageType]])
  extends System[Scene[ImageType]] {

  override def evolve: Scene[ImageType] =
    sprites.foldLeft(scene)((scene, sprite) => scene.withSprite(sprite))
}
