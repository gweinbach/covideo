package com.ezoky.ezgames.covideo.component

import com.ezoky.ezgames.covideo.assets.Asset

/**
 * @author gweinbach on 03/01/2022
 * @since 0.2.0
 */
trait Sprites[T: Dimension]
  extends Positions[T]:

  import CoordsDimension.*
  
  trait Sprite:
  
    type ImageType
    val image: ImageType

    val definition: SpriteDefinition
    val previousPosition: Option[Position]
    val position: Position
    
    def moveTo(position: Position): Sprite
    

  case class SpriteDefinition(image: Asset,
                              position: Position = Position.Zero,
                              previousPosition: Option[Position] = None)
    extends Sprite:
    
    override type ImageType = Asset

    override val definition: SpriteDefinition = this

    override def moveTo(position: Position): SpriteDefinition =
      copy(
        position = position,
        previousPosition = Some(this.position)
      )
      
  
