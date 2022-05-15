package com.ezoky.ezgames.covideo.component

trait Generator:
  def generateLong: (Long, Generator)
  def generateDouble: (Double, Generator)

type Generated[+A] = Generator => (A, Generator)

extension [A](generated: Generated[A])
  def map[B](f: A => B): Generated[B] =
    Generated.map(generated)(f)

  def flatMap[B](f: A => Generated[B]) =
    Generated.flatMap(generated)(f)

  def get(generator: Generator): A =
    generated(generator)._1

val GeneratedDouble: Generated[Double] =
  _.generateDouble

val GeneratedLong: Generated[Long] =
  _.generateLong


object Generated:

  def unit[A](a: A): Generated[A] =
    (gen: Generator) => (a, gen)

  def map[A, B](generated: Generated[A])(f: A => B): Generated[B] =
    (seed: Generator) =>
      val (a, nextGen) = generated(seed)
      (f(a), nextGen)

  def map2[A1, A2, B](generatedA1: Generated[A1], generatedA2: Generated[A2])(f: (A1, A2) => B): Generated[B] =
    (genA1: Generator) =>
      val (a1, genA2) = generatedA1(genA1)
      val (a2, nextGen) = generatedA2(genA2)
      (f(a1, a2), nextGen)

  def map3[A1, A2, A3,B](generatedA1: Generated[A1],
                         generatedA2: Generated[A2],
                         generatedA3: Generated[A3])(f: (A1, A2, A3) => B): Generated[B] =
    (genA1: Generator) =>
      val (a1, genA2) = generatedA1(genA1)
      val (a2, genA3) = generatedA2(genA2)
      val (a3, nextGen) = generatedA3(genA3)
      (f(a1, a2, a3), nextGen)

  def flatMap[A,B](generated: Generated[A])(f: A => Generated[B]): Generated[B] =
    (seed: Generator) =>
      val (a, nextGen) = generated(seed)
      f(a)(nextGen)

  def lift[A, B](f: A => B): Generated[A] => Generated[B] =
    generatedA => map(generatedA)(f)
    
  def setT[A](sGen: Set[Generated[A]]): Generated[Set[A]] =
    (seed: Generator) =>
      sGen.foldLeft((Set.empty[A], seed)){
        case ((set, gen), genA) =>
          val (a, nextGen) = genA(gen)
          (set + a, nextGen)
      }

  def foldSetGen[A, B](set: Set[A],
                       f: Generated[A] => Generated[B]): Generated[Set[B]] =
    foldSet(set.map(Generated.unit), f)

  def foldSet[A, B](set: Set[A],
                    f: A => Generated[B]): Generated[Set[B]] =
    (seed: Generator) =>
      set.foldLeft((Set.empty[B], seed)) {
        case ((setOfB, gen), a) =>
          val (nextB, nextGen) = f(a)(gen)
          (setOfB + nextB, nextGen)
      }

  def setOf[A](generated: Generated[A])(size: Int): Generated[Set[A]] =
    (seed: Generator) =>
      (0 until size).foldLeft((Set.empty[A], seed)) {
        case ((set, gen), _) =>
          val (a, nextGen) = generated(gen)
          (set + a, nextGen)
      }

  def listOf[A](generated: Generated[A])(size: Int): Generated[List[A]] =
    (seed: Generator) =>
      (0 until size).foldLeft((List.empty[A], seed)) {
        case ((list, gen), _) =>
          val (a, nextGen) = generated(gen)
          (list :+ a, nextGen)
      }


class RandomGenerator private(val seed: java.util.Random)
  extends Generator:

  private def this(previous: RandomGenerator) =
    this(previous.seed)

  def this(seed: Long) =
    this(new java.util.Random(seed))

  def this() =
    this(new java.util.Random())

  override def generateLong: (Long, Generator) =
    (seed.nextLong(), new RandomGenerator(this))

  override def generateDouble: (Double, Generator) =
    (seed.nextDouble(), new RandomGenerator((this)))
