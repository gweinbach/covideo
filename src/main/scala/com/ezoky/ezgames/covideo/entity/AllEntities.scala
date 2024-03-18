package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{AllComponents, Dimension, Identifiable}

//import spire.*
//import spire.implicits.*
//import spire.math.*


trait AllEntities[I: Identifiable, D: Dimension]
  extends Entities[I]
    with Worlds[I, D]
    with Scenes[I, D]
    with Games[I, D]
    with Persons[I, D]
    with Viewables[I, D]
    with AllComponents[D]
  