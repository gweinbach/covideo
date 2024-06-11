package com.ezoky.ezgames.covideo.component


trait AllComponents[D: Dimension]
  extends Mobiles[D]
    with Solids[D]
    with Sprites[D]