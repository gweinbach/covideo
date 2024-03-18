package com.ezoky.ezgames.covideo.component

trait Identifiable[I]:

  def id: I


import java.util.UUID

object UUIDIdentifiable extends Identifiable[UUID]:
  def id: UUID = UUID.randomUUID()
  