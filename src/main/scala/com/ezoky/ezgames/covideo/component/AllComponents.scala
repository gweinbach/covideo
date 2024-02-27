package com.ezoky.ezgames.covideo.component

import spire.algebra.Trig

trait Components[T: Dimension: Trig]
  extends Mobiles[T]
    with Solids[T]
    with Sprites[T]:
  
  val ComponentsDimension: Dimension[T] = CoordsDimension