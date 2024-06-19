package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.*
import com.ezoky.ezgames.covideo.component.Generate.Generated
import spire.*
import spire.implicits.*
import spire.math.*

trait Demographies[I: Identifiable]
  extends Entities[I]:

  trait PopulationDynamicsStrategy:
    def apply[A](toApply: () => Generated[Population[A]]): (PopulationDynamicsStrategy, Generated[Population[A]])

    def reset: PopulationDynamicsStrategy

  object PopulationDynamicsStrategy:

    /**
     * No population evolution
     */
    case object Zero extends PopulationDynamicsStrategy:
      def apply[A](toApply: () => Generated[Population[A]]): (Zero.type, Generated[Population[A]]) =
        (this, Generated.unit(Population.empty[A]))

      override def reset: Zero.type = this

    /**
     * Population evolves once then nothing happens until reset.
     *
     * @param armed
     */
    case class OneShot(armed: Boolean = true)
      extends PopulationDynamicsStrategy:

      override def apply[A](toApply: () => Generated[Population[A]]): (OneShot, Generated[Population[A]]) =
        armed match
          case true =>
            (OneShot(false), toApply())
          case false =>
            (this, Generated.unit(Population.empty[A]))

      override def reset: OneShot =
        OneShot(armed = true)

    /**
     * Always evolving.
     */
    case object Flat extends PopulationDynamicsStrategy:
      override def apply[A](toApply: () => Generated[Population[A]]): (Flat.type, Generated[Population[A]]) =
        (this, toApply())

      override def reset: Flat.type =
        this


  case class PopulationDynamics[E <: Entity](strategy: PopulationDynamicsStrategy,
                                             populationSelection: Generated[Population[E]] => Generated[Population[E]]):

    def evolve(population: Generated[Population[E]]): (PopulationDynamics[E], Generated[Population[E]]) =
      val evolution = strategy(() => populationSelection(population))
      (copy(strategy = evolution._1), evolution._2)

    def withStrategy(newStrategy: PopulationDynamicsStrategy): PopulationDynamics[E] =
      copy(strategy = newStrategy)

    def resetStrategy: PopulationDynamics[E] =
      copy(strategy = strategy.reset)

  object PopulationDynamics:

    def NoEvolution[E <: Entity]: PopulationDynamics[E] =
      PopulationDynamics(
        strategy = PopulationDynamicsStrategy.Zero,
        populationSelection = _ => Generated.unit(Population.empty[E])
      )

    def RandomBirth[E <: Entity](birthRate: Rate,
                                 strategy: PopulationDynamicsStrategy,
                                 beBorn: => Generated[E]): PopulationDynamics[E] =
      PopulationDynamics(
        strategy = strategy,
        populationSelection =
          (genPopulation: Generated[Population[E]]) =>
            for
              population <- genPopulation
              newBorn <- Generated.setOf(beBorn)(birthRate(population.number))
            yield
              Population(newBorn)
      )

    def RandomDeath[E <: Entity](deathRate: Rate,
                                 strategy: PopulationDynamicsStrategy): PopulationDynamics[E] =
      PopulationDynamics(
        strategy = strategy,
        populationSelection =
          (genPopulation: Generated[Population[E]]) =>
            for
              population <- genPopulation
              deadSet <- Generated.setOf(Generate.generatedBetweenIntegral(0, population.number - 1))(deathRate(population.number))
            yield
              Population(deadSet.map(deadIndex => population.indexOf(deadIndex)))
      )


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
      
    def withBirthStrategy(birthStrategy: PopulationDynamicsStrategy): Demography[E] =
      copy(birth = birth.withStrategy(birthStrategy))

    def withDeathStrategy(deathStrategy: PopulationDynamicsStrategy): Demography[E] =
      copy(death = death.withStrategy(deathStrategy))

    def resetBirthStrategy: Demography[E] =
      copy(birth = birth.resetStrategy)

    def resetDeathStrategy: Demography[E] =
      copy(death = death.resetStrategy)

  case class DemographyConfig[C](populationSize: Int,
                                 populationConfig: C,
                                 birthRate: Rate,
                                 deathRate: Rate)