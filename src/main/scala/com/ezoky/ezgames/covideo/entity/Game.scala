/*
 * Copyright (c) 2020 EZOKY
 */

package com.ezoky.ezgames.covideo.entity

/**
 * @author gweinbach on 14/11/2020
 * @since 0.1.0
 */
case class Game[ImageType](world: World[ImageType],
                           people: Set[Person[ImageType]])
