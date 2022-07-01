/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezcategory.IO
import com.ezoky.ezgames.covideo.component.Dimension.Ez3D.{Basis, Parallelepiped, SpacePoint}
import com.ezoky.ezgames.covideo.component.Dimension.{Geometry, SizeValue, Two, Zero}
import com.ezoky.ezgames.covideo.component.{Box, Sprite}
import com.ezoky.ezgames.covideo.entity.People.{PersonId, Population}

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class World(area: Box,
                 scene: Scene,
                 id: PersonId = PersonId()):

  def withScene(scene: Scene): World =
    copy(scene = scene.withComponents(worldBoundaries))

  lazy val worldBoundaries: Population[Component3D] =
    val boundaries =
      Parallelepiped(
        width = area.width.value.baseValue,
        height = area.height.value.baseValue,
        depth = area.depth.value.baseValue
      )
    Population(
      id -> Component3D(
        SpacePoint(area.width.value.baseValue / Two, area.height.value.baseValue / 2, -boundaries.depth / Two),
        Basis.Normal,
        boundaries
      )
    )

case class WorldConfig(areaConfig: AreaConfig,
                       sceneConfig: SceneConfig)


case class AreaConfig(width: SizeValue,
                      xGeometry: Geometry,
                      height: SizeValue,
                      yGeometry: Geometry,
                      depth: SizeValue,
                      zGeometry: Geometry)

