package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Generate.Generated
import spire.*
import spire.implicits.*
import spire.math.*

trait Demographies[I: Identifiable]
  extends Entities[I]:

  trait PopulationDynamicsProfile:
    def apply[A](toApply: () => Generated[Population[A]]): (PopulationDynamicsProfile, Generated[Population[A]])

    def reset: PopulationDynamicsProfile

    protected lazy val profile: LazyList[Boolean]

    infix def +(other: PopulationDynamicsProfile): PopulationDynamicsProfile =
      PopulationDynamicsProfile.add(this, other)

    infix def *(other: PopulationDynamicsProfile): PopulationDynamicsProfile =
      PopulationDynamicsProfile.mult(this, other)

  object PopulationDynamicsProfile:

    /**
     * No population evolution
     */
    case object Zero extends PopulationDynamicsProfile:
      def apply[A](toApply: () => Generated[Population[A]]): (Zero.type, Generated[Population[A]]) =
        (this, Generated.unit(Population.empty[A]))

      override def reset: Zero.type = this

      override protected lazy val profile: LazyList[Boolean] = LazyList()

    def add(profile1: PopulationDynamicsProfile,
            profile2: PopulationDynamicsProfile): PopulationDynamicsProfile =
      Variable.once(profile1.profile.zipAll(profile2.profile, false, false).map(p => p._1 || p._2))

    def mult(profile1: PopulationDynamicsProfile,
             profile2: PopulationDynamicsProfile): PopulationDynamicsProfile =
      Variable.once(profile1.profile.zipAll(profile2.profile, false, false).map(p => p._1 && p._2))

    /**
     * Population evolves once then nothing happens until reset.
     *
     * @param armed
     */
    case class OneShot(armed: Boolean = true)
      extends PopulationDynamicsProfile:

      override def apply[A](toApply: () => Generated[Population[A]]): (OneShot, Generated[Population[A]]) =
        armed match
          case true =>
            (OneShot(false), toApply())
          case false =>
            (this, Generated.unit(Population.empty[A]))

      override def reset: OneShot =
        OneShot(armed = true)

      override protected lazy val profile: LazyList[Boolean] =
        true #:: LazyList.continually(false)


    /**
     * Always evolving.
     */
    case object Flat extends PopulationDynamicsProfile:
      override def apply[A](toApply: () => Generated[Population[A]]): (Flat.type, Generated[Population[A]]) =
        (this, toApply())

      override def reset: Flat.type =
        this

      override protected lazy val profile: LazyList[Boolean] =
        LazyList.continually(true)


    case class Variable private(currentProfile: LazyList[Boolean],
                                initialProfile: LazyList[Boolean],
                                autoResetOnEnd: Boolean) extends PopulationDynamicsProfile:
      override def apply[A](toApply: () => Generated[Population[A]]): (Variable, Generated[Population[A]]) =
        currentProfile match
          case true #:: profileTail =>
            (Variable(profileTail, initialProfile, autoResetOnEnd), toApply())
          case false #:: profileTail =>
            (Variable(profileTail, initialProfile, autoResetOnEnd), Generated.unit(Population.empty[A]))
          case LazyList() =>
            if (!autoResetOnEnd) || initialProfile.isEmpty then
              (this, Generated.unit(Population.empty[A]))
            else
              reset.apply(toApply)

      override def reset: Variable =
        Variable(initialProfile, initialProfile, autoResetOnEnd)

      override protected lazy val profile: LazyList[Boolean] =
        if autoResetOnEnd then
          LazyList.continually(initialProfile).flatten
        else
          initialProfile

    object Variable:

      def apply(initialProfile: LazyList[Boolean]): Variable =
        repeat(initialProfile)

      def repeat(initialProfile: LazyList[Boolean]): Variable =
        new Variable(initialProfile, initialProfile, autoResetOnEnd = true)

      def once(initialProfile: LazyList[Boolean]): Variable =
        new Variable(initialProfile, initialProfile, autoResetOnEnd = false)

  
  
  /**
   *
   * @param profile
   * @param populationSelection
   * @tparam E
   */
  case class PopulationDynamics[E <: Entity](profile: PopulationDynamicsProfile,
                                             populationSelection: Generated[Population[E]] => Generated[Population[E]]):

//    def evolve(population: Generated[Population[E]]): (PopulationDynamics[E], Generated[Population[E]]) =
//      val selected = selectForEvolution(population)
//      (
//        selected._1,
//        for
//          initialPopulation <- population
//          selectedPopulation <- selected._2
//        yield
//          selectedPopulation._1 match
//            case Birth =>
//              initialPopulation ++ selectedPopulation
//            case Death =>
//              initialPopulation -- selectedPopulation
//      )
        
    def evolve(population: Generated[Population[E]]): (PopulationDynamics[E], Generated[Population[E]]) =
      val evolution = profile(() => populationSelection(population))
      (withProfile(evolution._1), evolution._2)

    def resetProfile: PopulationDynamics[E] =
      withProfile(profile.reset)
      
    def withProfile(newProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      copy(profile = newProfile)


//    infix def +(other: PopulationDynamics[E]): PopulationDynamics[E] =
//      PopulationDynamics.Add(this, other)
//
//    infix def -(other: PopulationDynamics[E]): PopulationDynamics[E] =
//      PopulationDynamics.Substract(this, other)
//
//    def unary_- : PopulationDynamics[E] =
//      PopulationDynamics.Minus(this)


  object PopulationDynamics:

    def NoEvolution[E <: Entity]: PopulationDynamics[E] =
      PopulationDynamics(
        profile = PopulationDynamicsProfile.Zero,
        populationSelection = _ => Generated.unit(Population.empty[E])
      )

//    def Add[E <: Entity](dynamics1: PopulationDynamics[E],
//                         dynamics2: PopulationDynamics[E]): PopulationDynamics[E] =
//      PopulationDynamics(
//        profile = dynamics1.profile + dynamics2.profile,
//        populationSelection =
//          (genPopulation: Generated[Population[E]]) =>
//            for
//              populationSelection1 <- dynamics1.populationSelection(genPopulation)
//              populationSelection2 <- dynamics2.populationSelection(genPopulation)
//            yield
//              populationSelection1 ++ populationSelection2
//      )
//
//    def Substract[E <: Entity](dynamics1: PopulationDynamics[E],
//                               dynamics2: PopulationDynamics[E]): PopulationDynamics[E] =
//      PopulationDynamics(
//        profile = dynamics1.profile + dynamics2.profile,
//        populationSelection =
//          (genPopulation: Generated[Population[E]]) =>
//            for
//              populationSelection1 <- dynamics1.populationSelection(genPopulation)
//              populationSelection2 <- dynamics2.populationSelection(genPopulation)
//            yield
//              populationSelection1 -- populationSelection2
//      )
//
//    def Minus[E <: Entity](dynamics: PopulationDynamics[E]): PopulationDynamics[E] =
//      Substract(
//        NoEvolution[E],
//        dynamics
//      )

    def RandomBirth[E <: Entity](birthRate: Rate,
                                 profile: PopulationDynamicsProfile,
                                 beBorn: => Generated[E]): PopulationDynamics[E] =
      PopulationDynamics(
        profile = profile,
        populationSelection =
          (genPopulation: Generated[Population[E]]) =>
            for
              population <- genPopulation
              newBorn <- Generated.setOf(beBorn)(birthRate(population.number))
            yield
              Population(newBorn)
      )

    def RandomDeath[E <: Entity](deathRate: Rate,
                                 profile: PopulationDynamicsProfile): PopulationDynamics[E] =
      //      Substract(
      //        NoEvolution[E],
      PopulationDynamics(
        profile = profile,
        populationSelection =
          (genPopulation: Generated[Population[E]]) =>
            for
              population <- genPopulation
              deadSet <- Generated.setOf(Generate.generatedBetweenIntegral(0, population.number - 1))(deathRate(population.number))
            yield
              Population(deadSet.map(deadIndex => population.indexOf(deadIndex)))
      )
  //      )


  case class Demography[E <: Entity](population: Population[E],
                                     birth: PopulationDynamics[E],
                                     death: PopulationDynamics[E]):

    def evolve: Generated[Demography[E]] =
      val afterDeath = death.evolve(Generated.unit(population))
      val afterBirth = birth.evolve(Generated.unit(population))
      for
        deadPopulation <- afterDeath._2
        bornPopulation <- afterBirth._2
      yield
        copy(
          population = (population -- deadPopulation) ++ bornPopulation,
          birth = afterBirth._1,
          death = afterDeath._1
        )

    def withPopulation(population: Population[E]): Demography[E] =
      copy(population = population)

    def withBirthProfile(birthProfile: PopulationDynamicsProfile): Demography[E] =
      copy(birth = birth.withProfile(birthProfile))

    def withDeathProfile(deathProfile: PopulationDynamicsProfile): Demography[E] =
      copy(death = death.withProfile(deathProfile))

    def resetBirthProfile: Demography[E] =
      copy(birth = birth.resetProfile)

    def resetDeathProfile: Demography[E] =
      copy(death = death.resetProfile)

  case class DemographyConfig[C](populationSize: Int,
                                 populationConfig: C,
                                 birthRate: Rate,
                                 deathRate: Rate)