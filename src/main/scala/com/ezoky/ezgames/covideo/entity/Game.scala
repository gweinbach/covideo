/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.entity.People.{Person, PersonConfig, Population}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Game(world: World,
                people: Population[Person]):
  
  def withWorld(world: World): Game =
    copy(world = world)


case class GameConfig(populationSize: Int,
                      personConfig: PersonConfig,
                      worldConfig: WorldConfig)
