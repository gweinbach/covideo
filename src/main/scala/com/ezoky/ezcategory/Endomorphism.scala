package com.ezoky.ezcategory

@FunctionalInterface
trait Endomorphism[A]
  extends Function1[A,A]

object Endomorphism :

  def apply[A](f: A => A): Endomorphism[A] =
    (a: A) => f(a)

  def identity[A]: Endomorphism[A] = Endomorphism(a => a)

