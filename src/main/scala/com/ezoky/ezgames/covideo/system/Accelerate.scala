package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.Games

trait Accelerate[T]:
  extension (entity: T) def accelerate: T

trait Accelerates[I: Identifiable, D: Dimension]
  extends Games[I, D]:

  given Accelerate[Population[Person]] with
    extension (people: Population[Person]) 
      override def accelerate: Population[Person] =
        people.map(_.accelerate)
        
  given Accelerate[Game] with
    extension (game: Game)
      override def accelerate: Game =
        game.withPeople(
          game.people.accelerate
        )



