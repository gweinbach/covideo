/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Dimension.{Geometry, SizeValue}
import com.ezoky.ezgames.covideo.component.{Box, Sprite}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class World(area: Box,
                 scene: Scene)


case class WorldConfig(areaConfig: AreaConfig,
                       sceneConfig: SceneConfig)


case class AreaConfig(width: SizeValue,
                      xGeometry: Geometry,
                      height: SizeValue,
                      yGeometry: Geometry,
                      depth: SizeValue,
                      zGeometry: Geometry)

