package com.ezoky.ezgames.covideo.system

import com.ezoky.ezgames.covideo.component.Generate.Generated
import com.ezoky.ezgames.covideo.component.{Dimension, Identifiable}
import com.ezoky.ezgames.covideo.entity.{Demographics, Games}

trait BeBornAndDie[T]:
  extension (generated: Generated[T])
    def beBornAndDie: Generated[T]


trait BeBornAndDies[I: Identifiable, D: Dimension]
  extends Games[I, D]
    with Demographics[I]:

  given BeBornAndDie[Demography[Person]] with
    extension (demography: Generated[Demography[Person]])
      override def beBornAndDie: Generated[Demography[Person]] =
        demography.flatMap(_.evolve)
  
  given BeBornAndDie[Game] with
    extension (generatedGame: Generated[Game])
      override def beBornAndDie: Generated[Game] =
        for 
          game <- generatedGame
          evolvedDemography <- Generated(game.demography).beBornAndDie
        yield
          game.withDemography(
            evolvedDemography
          )
    
  
