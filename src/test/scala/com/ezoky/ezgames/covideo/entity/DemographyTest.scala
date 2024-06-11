package com.ezoky.ezgames.covideo.entity

import com.ezoky.ezgames.covideo.component.Generate.*
import com.ezoky.ezgames.covideo.component.{Generate, Identifiable, Rate, UUIDIdentifiable}
import org.scalatest.flatspec.AnyFlatSpec

import java.util.UUID

class DemographyTest extends AnyFlatSpec:

  given Identifiable[UUID] = UUIDIdentifiable

  object TestDemographies extends Demographies[UUID] {}

  import TestDemographies.*

  case class IdedInt(id: UUID,
                     ided: Int) extends Entity

  object IdedInt:
    def apply(ided: Int): IdedInt =
      IdedInt(UUID.randomUUID(), ided)

    def fill(n: Int): List[IdedInt] =
      List.tabulate(n)(IdedInt(_))

  extension (population: Population[IdedInt])
    def ideds: Set[Int] =
      population.values.map(_.ided).toSet

  "birth" should "generate new entities" in {

    val initialPopulation = Population(IdedInt.fill(10))
    val oneShotEvolutionDemography = Demography(
      population = initialPopulation,
      birth = PopulationDynamics.RandomBirth(
        Rate(0.2),
        PopulationDynamicsStrategy.OneShot(),
        summon[Generated[Int]].map(IdedInt(_))
      ),
      death = PopulationDynamics.NoEvolution[IdedInt]
    )

    val generator = SequenceGenerator(100)

    val evolvedDemography = oneShotEvolutionDemography.evolve(generator)
    assert(evolvedDemography._1.population.number === 12)
    assert(evolvedDemography._1.population.toSet.map(_.ided) === Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 100, 101))

    val nextDemography = evolvedDemography._1.evolve(evolvedDemography._2)
    assert(nextDemography._1 == evolvedDemography._1, "Nothing evolves after first evolution in OneShot strategy")

    val nextResetDemography = evolvedDemography._1.resetBirthStrategy.evolve(evolvedDemography._2)
    assert(nextResetDemography._1.population.number === 14)
    assert(nextResetDemography._1.population.ideds === Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 100, 101, 102, 103), "A OneShot strategy can be reset")

    val flatEvolutionDemography = oneShotEvolutionDemography.withBirthStrategy(PopulationDynamicsStrategy.Flat)
    val evolvedFlatDemography = flatEvolutionDemography.evolve(generator)
    assert(evolvedFlatDemography._1.population.ideds === nextDemography._1.population.ideds, "At first step, Flat strategy and OneShot strategies are the same")
    val nextFlatDemography = evolvedFlatDemography._1.evolve(evolvedFlatDemography._2)
    assert(nextFlatDemography._1.population.ideds === nextResetDemography._1.population.ideds, "At second step a Flat strategy still evolves")

    val zeroEvolutionDemography = oneShotEvolutionDemography.withBirthStrategy(PopulationDynamicsStrategy.Zero)
    val evolvedZeroEvolutionDemography = zeroEvolutionDemography.evolve(generator)
    assert(evolvedZeroEvolutionDemography._1.population.ideds === initialPopulation.ideds, "A Zero strategy never evolves")
  }

  "death" should "remove entities" in {

    val initialPopulation = Population(IdedInt.fill(10))
    val oneShotEvolutionDemography = Demography(
      population = initialPopulation,
      birth = PopulationDynamics.NoEvolution[IdedInt],
      death = PopulationDynamics.RandomDeath(
        Rate(0.2),
        PopulationDynamicsStrategy.OneShot()
      )
    )

    val generator = SequenceGenerator(5)

    val evolvedDemography = oneShotEvolutionDemography.evolve(generator)
    assert(evolvedDemography._1.population.number === 8)

    val nextDemography = evolvedDemography._1.evolve(evolvedDemography._2)
    assert(nextDemography._1 == evolvedDemography._1, "Nothing evolves after first evolution in OneShot strategy")

    val nextResetDemography = evolvedDemography._1.resetDeathStrategy.evolve(evolvedDemography._2)
    assert(nextResetDemography._1.population.number === 7, "A OneShot strategy can be reset")

    val flatEvolutionDemography = oneShotEvolutionDemography.withDeathStrategy(PopulationDynamicsStrategy.Flat)
    val evolvedFlatDemography = flatEvolutionDemography.evolve(generator)
    assert(evolvedFlatDemography._1.population.number === nextDemography._1.population.number, "At first step, Flat strategy and OneShot strategies are the same")
    val nextFlatDemography = evolvedFlatDemography._1.evolve(evolvedFlatDemography._2)
    assert(nextFlatDemography._1.population.number === nextResetDemography._1.population.number, "At second step a Flat strategy still evolves")

    val zeroEvolutionDemography = oneShotEvolutionDemography.withDeathStrategy(PopulationDynamicsStrategy.Zero)
    val evolvedZeroEvolutionDemography = zeroEvolutionDemography.evolve(generator)
    assert(evolvedZeroEvolutionDemography._1.population.number === initialPopulation.number, "A Zero strategy never evolves")
  }

  "equivalent birth rate and death rate with Flat evolution strategy" should "keep population size stable" in {

    val initialPopulation = Population(IdedInt.fill(10))
    val flatEvolutionDemography = Demography(
      population = initialPopulation,
      birth = PopulationDynamics.RandomBirth(
        Rate(0.2),
        PopulationDynamicsStrategy.Flat,
        summon[Generated[Int]].map(IdedInt(_))
      ),
      death = PopulationDynamics.RandomDeath(
        Rate(0.2),
        PopulationDynamicsStrategy.Flat
      )
    )
    println(initialPopulation.ideds)

    val generator = SequenceGenerator(100)

    val step1Demography = flatEvolutionDemography.evolve(generator)
    println(step1Demography._1.population.ideds)
    assert(step1Demography._1.population.number === initialPopulation.number)

    val step2Demography = step1Demography._1.evolve(step1Demography._2)
    println(step2Demography._1.population.ideds)
    assert(step2Demography._1.population.number == initialPopulation.number)

    val step3Demography = step2Demography._1.evolve(step2Demography._2)
    println(step3Demography._1.population.ideds)
    assert(step3Demography._1.population.number == initialPopulation.number)
  }