/*
 * @author gweinbach on $today.date
 * @since 0.2.0
 *
 */

package com.ezoky.ez3d

/**
 * @since 0.2.0
 * @author gweinbach on 11/06/2022
 */
trait Transformation[V]
  extends (V => V) :

  def transform(v: V): V

  final override def apply(v: V): V =
    transform(v)

  infix def o(t2: Transformation[V]): Transformation[V] =
    (v: V) => transform(t2.transform(v))


class Identity[V]
  extends Transformation[V] :

  final override def transform(v: V): V = v


trait Rotation[V]
  extends Transformation[V] :

  final override def transform(v: V): V = rotate(v)

  def rotate(v: V): V


trait Translation[V]
  extends Transformation[V] :

  final override def transform(v: V): V = translate(v)

  def translate(v: V): V


extension [V](v: V)

  final def rotate(using rotation: Rotation[V]): V =
    rotation.rotate(v)

  final def translate(using translation: Translation[V]): V =
    translation.translate(v)
