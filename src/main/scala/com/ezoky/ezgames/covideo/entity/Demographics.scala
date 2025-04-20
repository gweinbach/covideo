package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Generate.Generated
import spire.*
import spire.implicits.*
import spire.math.*

import scala.annotation.targetName

trait Demographics[I: Identifiable]
  extends Entities[I]:

  trait PopulationDynamicsProfile:
    def apply[A](toApply: () => Generated[Population[A]]): (PopulationDynamicsProfile, Generated[Population[A]])

    def reset: PopulationDynamicsProfile

    protected lazy val profile: LazyList[Boolean]

    @targetName("orProfile")
    infix def or(other: PopulationDynamicsProfile): PopulationDynamicsProfile =
      PopulationDynamicsProfile.or(this, other)

    @targetName("andProfile")
    infix def and(other: PopulationDynamicsProfile): PopulationDynamicsProfile =
      PopulationDynamicsProfile.and(this, other)

  object PopulationDynamicsProfile:

    /**
     * No population evolution
     */
    case object Zero extends PopulationDynamicsProfile:
      def apply[A](toApply: () => Generated[Population[A]]): (Zero.type, Generated[Population[A]]) =
        (this, Generated.unit(Population.empty[A]))

      override def reset: Zero.type = this

      override protected lazy val profile: LazyList[Boolean] = LazyList()

    def or(profile1: PopulationDynamicsProfile,
           profile2: PopulationDynamicsProfile): PopulationDynamicsProfile =
      Variable.once(profile1.profile.zipAll(profile2.profile, false, false).map(p => p._1 || p._2))

    def and(profile1: PopulationDynamicsProfile,
            profile2: PopulationDynamicsProfile): PopulationDynamicsProfile =
      Variable.once(profile1.profile.zipAll(profile2.profile, false, false).map(p => p._1 && p._2))

    /**
     * Population evolves once then nothing happens until reset.
     *
     * @param armed if true transformation will work, if false not
     */
    case class OneShot(armed: Boolean = true)
      extends PopulationDynamicsProfile:

      override def apply[A](toApply: () => Generated[Population[A]]): (OneShot, Generated[Population[A]]) =
        if armed then
          (OneShot(false), toApply())
        else
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

      def thenWait(stepNumber: Int): Variable =
        copy(initialProfile = initialProfile ++ LazyList.fill(stepNumber)(false))

      def thenRestart(stepNumber: Int): Variable =
        copy(initialProfile = initialProfile ++ LazyList.fill(stepNumber)(true))

      override protected lazy val profile: LazyList[Boolean] =
        if autoResetOnEnd then
          LazyList.continually(initialProfile).flatten
        else
          initialProfile

    object Variable:

      def apply(initialProfile: LazyList[Boolean]): Variable =
        repeat(initialProfile)

      def apply(n: Int): Variable =
        apply(LazyList.fill(n)(true))

      def repeat(initialProfile: LazyList[Boolean]): Variable =
        new Variable(initialProfile, initialProfile, autoResetOnEnd = true)

      def once(initialProfile: LazyList[Boolean]): Variable =
        new Variable(initialProfile, initialProfile, autoResetOnEnd = false)

  private type PopulationCombinator[E <: Entity] = (Population[E], Population[E]) => Population[E]

  sealed trait PopulationDynamics[E <: Entity]:

    final def evolve(initialPopulation: Generated[Population[E]]): (PopulationDynamics[E], Generated[Population[E]]) =
      applyEvolutions(initialPopulation, describeEvolutions(initialPopulation))

    private def applyEvolutions(initialPopulation: Generated[Population[E]],
                                evolutions: List[(PopulationDynamics[E], Generated[Population[E]], PopulationCombinator[E])]): (PopulationDynamics[E], Generated[Population[E]]) =
      evolutions match
        case Nil =>
          (NoEvolution[E](), initialPopulation)
        case (evolvedDynamics, selectedPopulation, populationCombinator) :: otherEvolutions =>
          val (otherDynamics, evolvedFromOthers) =
            applyEvolutions(initialPopulation, otherEvolutions)
          (
            evolvedDynamics + otherDynamics,
            for
              population1 <- evolvedFromOthers
              population2 <- selectedPopulation
            yield
              val combinedPopulation = populationCombinator(population1, population2)
              //              println(s"pop1(${population1.number})=${population1.values}")
              //              println(s"pop2(${population2.number})=${population2.values}")
              //              println(s"popC(${combinedPopulation.size})=${combinedPopulation.values}")
              combinedPopulation
          )

    def describeEvolutions(initialPopulation: Generated[Population[E]]): List[(PopulationDynamics[E], Generated[Population[E]], PopulationCombinator[E])]

    def resetProfile(): PopulationDynamics[E]

    def withBirthProfile(birthProfile: PopulationDynamicsProfile): PopulationDynamics[E]

    def withDeathProfile(deathProfile: PopulationDynamicsProfile): PopulationDynamics[E]

    @targetName("composeDynamics")
    infix def +(other: PopulationDynamics[E]): PopulationDynamics[E] =
      PopulationDynamics(this, other)

  /**
   *
   * @param profile
   * @param populationSelection
   * @param populationEvolution
   * @tparam E
   */
  abstract class MonotonousPopulationDynamics[E <: Entity](profile: PopulationDynamicsProfile,
                                                           populationSelection: Generated[Population[E]] => Generated[Population[E]],
                                                           populationEvolution: (Population[E], Population[E]) => Population[E])
    extends PopulationDynamics[E]:

    override def describeEvolutions(initialPopulation: Generated[Population[E]]): List[(PopulationDynamics[E], Generated[Population[E]], PopulationCombinator[E])] =
      val selection = selectPopulation(initialPopulation)
      List((selection._1, selection._2, populationEvolution))

    private def selectPopulation(population: Generated[Population[E]]): (PopulationDynamics[E], Generated[Population[E]]) =
      val evolution = profile(() => populationSelection(population))
      (withProfile(evolution._1), evolution._2)

    override def resetProfile(): PopulationDynamics[E] =
      withProfile(profile.reset)

    def withProfile(newProfile: PopulationDynamicsProfile): PopulationDynamics[E]


  case class Birth[E <: Entity](profile: PopulationDynamicsProfile,
                                populationSelection: Generated[Population[E]] => Generated[Population[E]])
    extends MonotonousPopulationDynamics[E](profile, populationSelection, _ ++ _):

    override def withProfile(newProfile: PopulationDynamicsProfile): Birth[E] =
      copy(profile = newProfile)

    override def withBirthProfile(birthProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      withProfile(birthProfile)

    override def withDeathProfile(deathProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      this


  case class Death[E <: Entity](profile: PopulationDynamicsProfile,
                                populationSelection: Generated[Population[E]] => Generated[Population[E]])
    extends MonotonousPopulationDynamics[E](profile, populationSelection, _ -- _):

    override def withProfile(newProfile: PopulationDynamicsProfile): Death[E] =
      copy(profile = newProfile)

    override def withBirthProfile(birthProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      this

    override def withDeathProfile(deathProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      withProfile(deathProfile)


  case class NoEvolution[E <: Entity]()
    extends PopulationDynamics[E]:

    override def describeEvolutions(initialPopulation: Generated[Population[E]]): List[(PopulationDynamics[E], Generated[Population[E]], PopulationCombinator[E])] =
      Nil

    override def resetProfile(): PopulationDynamics[E] =
      this

    override def withBirthProfile(birthProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      this

    override def withDeathProfile(deathProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      this


  case class ComposedDynamics[E <: Entity](dynamics1: PopulationDynamics[E],
                                           dynamics2: PopulationDynamics[E])
    extends PopulationDynamics[E]:

    override def describeEvolutions(initialPopulation: Generated[Population[E]]): List[(PopulationDynamics[E], Generated[Population[E]], PopulationCombinator[E])] =
      dynamics1.describeEvolutions(initialPopulation) ++ dynamics2.describeEvolutions(initialPopulation)

    override def resetProfile(): PopulationDynamics[E] =
      dynamics1.resetProfile() + dynamics2.resetProfile()

    override def withBirthProfile(birthProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      dynamics1.withBirthProfile(birthProfile) + dynamics2.withBirthProfile(birthProfile)

    override def withDeathProfile(deathProfile: PopulationDynamicsProfile): PopulationDynamics[E] =
      dynamics1.withDeathProfile(deathProfile) + dynamics2.withDeathProfile(deathProfile)


  object PopulationDynamics:

    def apply[E <: Entity](dynamics1: PopulationDynamics[E],
                           dynamics2: PopulationDynamics[E]): PopulationDynamics[E] =
      (dynamics1, dynamics2) match
        case (NoEvolution(), _) =>
          dynamics2
        case (_, NoEvolution()) =>
          dynamics1
        case _ =>
          ComposedDynamics(dynamics1, dynamics2)

    def randomBirth[E <: Entity](birthRate: Rate,
                                 profile: PopulationDynamicsProfile,
                                 beBorn: => Generated[E]): PopulationDynamics[E] =
      Birth(
        profile = profile,
        populationSelection =
          (genPopulation: Generated[Population[E]]) =>
            for
              population <- genPopulation
              newBorn <- Generated.setOf(beBorn)(birthRate(population.size))
            yield
              Population(newBorn)
      )

    def randomDeath[E <: Entity](deathRate: Rate,
                                 profile: PopulationDynamicsProfile): PopulationDynamics[E] =
      Death(
        profile = profile,
        populationSelection =
          (genPopulation: Generated[Population[E]]) =>
            for
              population <- genPopulation
              deadSet <- Generated.setOf(Generate.generatedBetweenIntegral(0, population.size - 1))(deathRate(population.size))
            yield
              Population(deadSet.map(deadIndex => population.indexOf(deadIndex)))
      )


  case class Demography[E <: Entity](population: Population[E],
                                     dynamics: PopulationDynamics[E]):

    def evolve: Generated[Demography[E]] =
      val afterEvolution = dynamics.evolve(Generated.unit(population))
      for
        evolvedPopulation <- afterEvolution._2
      yield
        copy(
          population = evolvedPopulation,
          dynamics = afterEvolution._1
        )

    def withPopulation(population: Population[E]): Demography[E] =
      copy(population = population)

    def withDynamics(dynamics: PopulationDynamics[E]): Demography[E] =
      copy(dynamics = dynamics)

    def resetProfile: Demography[E] =
      withDynamics(dynamics.resetProfile())

    def withBirthProfile(birthProfile: PopulationDynamicsProfile): Demography[E] =
      withDynamics(dynamics.withBirthProfile(birthProfile))

    def withDeathProfile(deathProfile: PopulationDynamicsProfile): Demography[E] =
      withDynamics(dynamics.withDeathProfile(deathProfile))
  //
  //    def resetBirthProfile: Demography[E] =
  //      copy(birth = birth.resetProfile)
  //
  //    def resetDeathProfile: Demography[E] =
  //      copy(death = death.resetProfile)

  case class DemographyConfig[C](populationSize: Int,
                                 populationConfig: C,
                                 birthRate: Rate,
                                 deathRate: Rate,
                                 birthProfileDescription: List[Int],
                                 deathProfileDescription: List[Int]) {

    val birthProfile = profileFromList(birthProfileDescription)

    val deathProfile = profileFromList(deathProfileDescription)

    private def profileFromList(profileList: List[Int]): PopulationDynamicsProfile =
      if profileList.isEmpty then
        PopulationDynamicsProfile.Zero
      else
        profileFromListTail(PopulationDynamicsProfile.Variable(profileList.head), profileList.tail, false)

    @tailrec
    private def profileFromListTail(headProfile: PopulationDynamicsProfile.Variable,
                                    profileListTail: List[Int],
                                    on: Boolean): PopulationDynamicsProfile =
      profileListTail match
        case Nil =>
          headProfile
        case length :: tail if on =>
          profileFromListTail(headProfile.thenRestart(length), tail, false)
        case length :: tail if !on =>
          profileFromListTail(headProfile.thenWait(length), tail, true)
  }