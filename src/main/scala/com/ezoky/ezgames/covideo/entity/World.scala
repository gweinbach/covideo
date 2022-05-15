/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.Dimension.Geometry
import com.ezoky.ezgames.covideo.component.{Area, Sprite}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class World(area: Area,
                 scene: Scene)


case class WorldConfig(areaConfig: AreaConfig,
                       sceneConfig: SceneConfig)


case class AreaConfig(xGeometry: Geometry,
                      yGeometry: Geometry,
                      zGeometry: Geometry)

