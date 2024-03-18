package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}

trait AllSystems[I: Identifiable, D: Dimension]
  extends Moves[I, D]
    with Accelerates[I, D]
    with AngularAccelerates[I, D]
    with Rotates[I, D]
    with Displays[I, D]
    with Evolves[I, D]
    with UserCommands[I, D]