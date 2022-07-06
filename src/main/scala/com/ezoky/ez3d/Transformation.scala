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
  extends (V => Option[V]) :

  def transform(v: V): Option[V]

  final override def apply(v: V): Option[V] =
    transform(v)

  infix def o(t2: Transformation[V]): Transformation[V] =
    (v: V) => t2.transform(v).flatMap(transform)


class Identity[V]
  extends Transformation[V] :

  final override def transform(v: V): Option[V] = Some(v)


trait Rotation[V]
  extends Transformation[V] :

  final override def transform(v: V): Option[V] = rotate(v)

  def rotate(v: V): Option[V]


trait Translation[V]
  extends Transformation[V] :

  final override def transform(v: V): Option[V] = Some(translate(v))

  def translate(v: V): V


extension [V](v: V)

  final def rotate(using rotation: Rotation[V]): Option[V] =
    rotation.rotate(v)

  final def translate(using translation: Translation[V]): V =
    translation.translate(v)
