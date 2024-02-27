/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.Generate.Generated
import com.ezoky.ezgames.covideo.component.{Components, Identifiable}
import com.ezoky.ezgames.covideo.component.double.Components.*
import spire.*
import spire.implicits.*

import scala.annotation.targetName


/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Persons[I: Identifiable: D: Dimension]
  extends Entites[I]
  with Components[D]:
  
  case class Person(id: I = summon[Identifiable[I]].id,
                    solid: Solid,
                    healthCondition: HealthCondition,
                    sprite: Sprite,
                    shape: Shape)
    extends Entity:

    def move: Person =
      val movedSolid = solid.move
      copy(
        solid = movedSolid,
        sprite = sprite.moveTo(solid.mobile.position)
      )

    def accelerate: Person =
      copy(solid = solid.accelerate)

    def turn(newAcceleration: Acceleration): Person =
      copy(solid = solid.turn(newAcceleration))

    def rotate: Person =
      copy(solid = solid.rotate)

    def angularAccelerate: Person =
      copy(solid = solid.angularAccelerate)

    def withSolid(solid: Solid): Person =
      copy(solid = solid)

  case class PersonConfig(shape: Generated[Shape],
                          solidConfig: SolidConfig)

  
