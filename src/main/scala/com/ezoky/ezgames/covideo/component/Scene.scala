package com.ezoky.ezgames.covideo.component

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Scene:

  def project(position: Position): (Int, Int)

  def withSprite(sprite: Sprite): Scene

  def withSprites(sprites: Iterable[Sprite]): Scene =
    sprites.foldLeft(this)(
      (scene, sprite) =>
        scene.withSprite(sprite)
    )

  def withArea(area: Area): Scene

