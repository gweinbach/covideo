package com.ezoky.ezgames.covideo.entity

trait Entity[I]:
  val id: I

  override def equals(obj: Any): Boolean =
    obj match
      case that: Entity[_] =>
        (that != null) && (that.id == this.id)
      case default =>
        false

  override def hashCode(): Int =
    id.hashCode()