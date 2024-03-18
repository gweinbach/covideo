/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{AllComponents, Dimension, Identifiable}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Games[I: Identifiable, D: Dimension]
  extends Worlds[I, D]
    with Persons[I, D]
    with Entities[I]
    with AllComponents[D]:

  import CoordsDimension.{*, given}
  
  case class Game(world: World,
                  people: Population[Person]):
  
    def withWorld(world: World): Game =
      copy(world = world)
  
    def withPeople(people: Population[Person]): Game =
      copy(people = people)
  
  case class GameConfig(populationSize: Int,
                        personConfig: PersonConfig,
                        worldConfig: WorldConfig)

