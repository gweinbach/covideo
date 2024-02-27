package com.ezoky.ezgames.covideo.component

trait Identifiable[I]:

  def identical[I2 <: I](other: I2): Boolean =
    other.id == id
    
  extension (identifiable: I)
    def id: I


import java.util.UUID

given Identifiable[UUID] with
  def id: UUID = UUID.randomUUID()
  