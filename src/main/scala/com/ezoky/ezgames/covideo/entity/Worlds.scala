/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.double.Components.*
import com.ezoky.ezgames.covideo.component.double.Components.Ez3D.*
import com.ezoky.ezgames.covideo.component.{Box, Identifiable, Sprite}
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
trait Worlds[I: Identifiable, D: Dimension]
  extends Entities[I]
  with Components[D]:
  
  case class World(area: Box,
                   scene: Scene,
                   id: I = summon[Identifiable[I]].id)
    extends Entity:
  
    def withScene(scene: Scene): World =
      copy(scene = scene)
  
  
  case class WorldConfig(areaConfig: AreaConfig,
                         sceneConfig: SceneConfig)
  
  case class AreaConfig(width: SizeValue,
                        xGeometry: Geometry,
                        height: SizeValue,
                        yGeometry: Geometry,
                        depth: SizeValue,
                        zGeometry: Geometry)
  
