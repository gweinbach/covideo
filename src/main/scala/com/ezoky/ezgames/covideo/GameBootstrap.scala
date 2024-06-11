package com.ezoky.ezgames.covideo

import com.ezoky.ezgames.covideo.component.swing.AllSwingComponents
import com.ezoky.ezgames.covideo.component.{AllComponents, Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.AllEntities
import com.ezoky.ezgames.covideo.system.AllSystems
import com.ezoky.ezgames.covideo.system.swing.AllSwingSystems

trait GameBootstrap[I: Identifiable, D: Dimension]
  extends AllComponents[D]
    with AllSwingComponents[D]
    with AllEntities[I, D]
    with AllSystems[I, D]
    with AllSwingSystems[I, D]
