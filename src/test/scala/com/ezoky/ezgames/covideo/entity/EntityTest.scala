package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.{Identifiable, UUIDIdentifiable}
import org.scalatest.flatspec.AnyFlatSpec

import java.util.UUID

class EntityTest extends AnyFlatSpec:

  given Identifiable[UUID] = UUIDIdentifiable

  object TestEntities extends Entities[UUID] {}

  import TestEntities.*

  "a population" can "be accessed as an indexed collection" in {

    val intPopulation = Population((UUID.randomUUID(), 1), (UUID.randomUUID(), 2), (UUID.randomUUID(), 3))
    assert(Set(intPopulation.indexOf(0), intPopulation.indexOf(1), intPopulation.indexOf(2)) === Set(1, 2, 3))
  }