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

  "Zero PopulationDynamicsStrategy" should "never produce any new population" in {

    val selectedPopulation = Population(IdedInt(0), IdedInt(1), IdedInt(3))
    val generator = SequenceGenerator(100)

    val appliedZero = PopulationDynamicsProfile.Zero(() => Generated(selectedPopulation))
    assert(appliedZero._1 === PopulationDynamicsProfile.Zero)
    assert(appliedZero._2.get(generator) === Population.empty[IdedInt])
  }

  "OneShot PopulationDynamicsStrategy" should "select population only once" in {

    val selectedPopulation = Population(IdedInt(0), IdedInt(1), IdedInt(3))
    val generator = SequenceGenerator(100)

    val appliedOneShot = PopulationDynamicsProfile.OneShot(true)(() => Generated(selectedPopulation))
    assert(appliedOneShot._1 === PopulationDynamicsProfile.OneShot(false))
    assert(appliedOneShot._2.get(generator) === selectedPopulation)

    val reappliedOneShot = PopulationDynamicsProfile.OneShot(false)(() => Generated(selectedPopulation))
    assert(reappliedOneShot._1 === PopulationDynamicsProfile.OneShot(false))
    assert(reappliedOneShot._2.get(generator) === Population.empty[IdedInt])

    val rearmed = PopulationDynamicsProfile.OneShot(false).reset
    assert(rearmed === PopulationDynamicsProfile.OneShot(true))
    val rearmedOneShot = rearmed(() => Generated(selectedPopulation))
    assert(rearmedOneShot._1 === PopulationDynamicsProfile.OneShot(false))
    assert(rearmedOneShot._2.get(generator) === selectedPopulation)
  }

  "Flat PopulationDynamicsStrategy" should "always select population" in {

    val selectedPopulation = Population(IdedInt(0), IdedInt(1), IdedInt(3))
    val generator = SequenceGenerator(100)

    val appliedFlat = PopulationDynamicsProfile.Flat(() => Generated(selectedPopulation))
    assert(appliedFlat._1 === PopulationDynamicsProfile.Flat)
    assert(appliedFlat._2.get(generator) === selectedPopulation)

    val reappliedFlat = appliedFlat._1(() => Generated(selectedPopulation))
    assert(reappliedFlat._1 === PopulationDynamicsProfile.Flat)
    assert(reappliedFlat._2.get(generator) === selectedPopulation)
  }

  "Variable PopulationDynamicsStrategy" can "be variable in time and repeat til the end of times" in {
    val selectedPopulation = Population(IdedInt(0), IdedInt(1), IdedInt(3))
    val generator = SequenceGenerator(100)

    val variableProfile = PopulationDynamicsProfile.Variable.repeat(LazyList(false, true, false, false, true))
    val appliedVariable1 = variableProfile(() => Generated(selectedPopulation))
    assert(appliedVariable1._2.get(generator) === Population.empty[IdedInt])

    val appliedVariable2 = appliedVariable1._1(() => Generated(selectedPopulation))
    assert(appliedVariable2._2.get(generator) === selectedPopulation)

    val appliedVariable3 = appliedVariable2._1(() => Generated(selectedPopulation))
    assert(appliedVariable3._2.get(generator) === Population.empty[IdedInt])

    val appliedVariable4 = appliedVariable3._1(() => Generated(selectedPopulation))
    assert(appliedVariable4._2.get(generator) === Population.empty[IdedInt])

    val appliedVariable5 = appliedVariable4._1(() => Generated(selectedPopulation))
    assert(appliedVariable5._2.get(generator) === selectedPopulation)

    // restart on end of list
    val appliedVariable6 = appliedVariable5._1(() => Generated(selectedPopulation))
    assert(appliedVariable6._2.get(generator) === Population.empty[IdedInt])

    val appliedVariable7 = appliedVariable6._1(() => Generated(selectedPopulation))
    assert(appliedVariable7._2.get(generator) === selectedPopulation)
  }

  "Variable PopulationDynamicsStrategy" can "be variable in time and stop when it's over" in {
    val selectedPopulation = Population(IdedInt(0), IdedInt(1), IdedInt(3))
    val generator = SequenceGenerator(100)

    val variableProfile = PopulationDynamicsProfile.Variable.once(LazyList(false, true, false, false, true))
    val appliedVariable1 = variableProfile(() => Generated(selectedPopulation))
    assert(appliedVariable1._2.get(generator) === Population.empty[IdedInt])

    val appliedVariable2 = appliedVariable1._1(() => Generated(selectedPopulation))
    assert(appliedVariable2._2.get(generator) === selectedPopulation)

    val appliedVariable3 = appliedVariable2._1(() => Generated(selectedPopulation))
    assert(appliedVariable3._2.get(generator) === Population.empty[IdedInt])

    val appliedVariable4 = appliedVariable3._1(() => Generated(selectedPopulation))
    assert(appliedVariable4._2.get(generator) === Population.empty[IdedInt])

    val appliedVariable5 = appliedVariable4._1(() => Generated(selectedPopulation))
    assert(appliedVariable5._2.get(generator) === selectedPopulation)

    // stop on end of list
    val appliedVariable6 = appliedVariable5._1(() => Generated(selectedPopulation))
    assert(appliedVariable6._2.get(generator) === Population.empty[IdedInt])

    val appliedVariable7 = appliedVariable6._1(() => Generated(selectedPopulation))
    assert(appliedVariable7._2.get(generator) === Population.empty[IdedInt])
  }

  "PopulationDynamicsStrategy" can "be composed" in {
    val selectedPopulation = Population(IdedInt(0), IdedInt(1), IdedInt(3))
    val generator = SequenceGenerator(100)

    def iterate(profile: PopulationDynamicsProfile,
                steps: Int): List[Boolean] =
      if steps == 0 then
        Nil
      else
        val applied = profile(() => Generated(selectedPopulation))
        (if applied._2.get(generator) == selectedPopulation then
          true
        else
          false) :: iterate(applied._1, steps - 1)

    val variableOnceProfile = PopulationDynamicsProfile.Variable.once(LazyList(false, true, false, false, true))
    val variableRepeatProfile = PopulationDynamicsProfile.Variable.repeat(LazyList(false, true, false, false, true))
    val zeroProfile = PopulationDynamicsProfile.Zero
    val oneShotProfile = PopulationDynamicsProfile.OneShot()
    val flatProfile = PopulationDynamicsProfile.Flat

    assert(iterate(variableOnceProfile, 11) === List(false, true, false, false, true, false, false, false, false, false, false))
    assert(iterate(variableRepeatProfile, 11) === List(false, true, false, false, true, false, true, false, false, true, false))

    assert(iterate(variableOnceProfile + zeroProfile, 11) === List(false, true, false, false, true, false, false, false, false, false, false))
    assert(iterate(variableOnceProfile + oneShotProfile, 11) === List(true, true, false, false, true, false, false, false, false, false, false))
    assert(iterate(variableOnceProfile + flatProfile, 11) === List(true, true, true, true, true, true, true, true, true, true, true))

    assert(iterate(variableRepeatProfile + zeroProfile, 11) === List(false, true, false, false, true, false, true, false, false, true, false))
    assert(iterate(variableRepeatProfile + oneShotProfile, 11) === List(true, true, false, false, true, false, true, false, false, true, false))
    assert(iterate(variableRepeatProfile + flatProfile, 11) === List(true, true, true, true, true, true, true, true, true, true, true))

    assert(iterate(variableOnceProfile + variableRepeatProfile, 11) === List(false, true, false, false, true, false, true, false, false, true, false))
    assert(iterate(variableOnceProfile + variableRepeatProfile + oneShotProfile, 11) === List(true, true, false, false, true, false, true, false, false, true, false))

    assert(iterate(variableOnceProfile * zeroProfile, 11) === List(false, false, false, false, false, false, false, false, false, false, false))
    assert(iterate(variableOnceProfile * oneShotProfile, 11) === List(false, false, false, false, false, false, false, false, false, false, false))
    assert(iterate(variableOnceProfile * flatProfile, 11) === List(false, true, false, false, true, false, false, false, false, false, false))

    assert(iterate(variableRepeatProfile * zeroProfile, 11) === List(false, false, false, false, false, false, false, false, false, false, false))
    assert(iterate(variableRepeatProfile * oneShotProfile, 11) === List(false, false, false, false, false, false, false, false, false, false, false))
    assert(iterate(variableRepeatProfile * flatProfile, 11) === List(false, true, false, false, true, false, true, false, false, true, false))

    assert(iterate(variableOnceProfile * variableRepeatProfile, 11) === List(false, true, false, false, true, false, false, false, false, false, false))
    assert(iterate(variableOnceProfile * variableRepeatProfile * oneShotProfile, 11) === List(false, false, false, false, false, false, false, false, false, false, false))
  }

  "birth" should "generate new entities" in {

    val initialPopulation = Population(IdedInt.fill(10))
    val oneShotEvolutionDemography = Demography(
      population = initialPopulation,
      birth = PopulationDynamics.RandomBirth(
        Rate(0.2),
        PopulationDynamicsProfile.OneShot(),
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

    val nextResetDemography = evolvedDemography._1.resetBirthProfile.evolve(evolvedDemography._2)
    assert(nextResetDemography._1.population.number === 14)
    assert(nextResetDemography._1.population.ideds === Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 100, 101, 102, 103), "A OneShot strategy can be reset")

    val flatEvolutionDemography = oneShotEvolutionDemography.withBirthProfile(PopulationDynamicsProfile.Flat)
    val evolvedFlatDemography = flatEvolutionDemography.evolve(generator)
    assert(evolvedFlatDemography._1.population.ideds === nextDemography._1.population.ideds, "At first step, Flat strategy and OneShot strategies are the same")
    val nextFlatDemography = evolvedFlatDemography._1.evolve(evolvedFlatDemography._2)
    assert(nextFlatDemography._1.population.ideds === nextResetDemography._1.population.ideds, "At second step a Flat strategy still evolves")

    val zeroEvolutionDemography = oneShotEvolutionDemography.withBirthProfile(PopulationDynamicsProfile.Zero)
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
        PopulationDynamicsProfile.OneShot()
      )
    )

    val generator = SequenceGenerator(5)

    val evolvedDemography = oneShotEvolutionDemography.evolve(generator)
    assert(evolvedDemography._1.population.number === 8)

    val nextDemography = evolvedDemography._1.evolve(evolvedDemography._2)
    assert(nextDemography._1 == evolvedDemography._1, "Nothing evolves after first evolution in OneShot strategy")

    val nextResetDemography = evolvedDemography._1.resetDeathProfile.evolve(evolvedDemography._2)
    assert(nextResetDemography._1.population.number === 7, "A OneShot strategy can be reset")

    val flatEvolutionDemography = oneShotEvolutionDemography.withDeathProfile(PopulationDynamicsProfile.Flat)
    val evolvedFlatDemography = flatEvolutionDemography.evolve(generator)
    assert(evolvedFlatDemography._1.population.number === nextDemography._1.population.number, "At first step, Flat strategy and OneShot strategies are the same")
    val nextFlatDemography = evolvedFlatDemography._1.evolve(evolvedFlatDemography._2)
    assert(nextFlatDemography._1.population.number === nextResetDemography._1.population.number, "At second step a Flat strategy still evolves")

    val zeroEvolutionDemography = oneShotEvolutionDemography.withDeathProfile(PopulationDynamicsProfile.Zero)
    val evolvedZeroEvolutionDemography = zeroEvolutionDemography.evolve(generator)
    assert(evolvedZeroEvolutionDemography._1.population.number === initialPopulation.number, "A Zero strategy never evolves")
  }

  "equivalent birth rate and death rate with Flat evolution strategy" should "keep population size stable" in {

    val initialPopulation = Population(IdedInt.fill(10))
    val flatEvolutionDemography = Demography(
      population = initialPopulation,
      birth = PopulationDynamics.RandomBirth(
        Rate(0.2),
        PopulationDynamicsProfile.Flat,
        summon[Generated[Int]].map(IdedInt(_))
      ),
      death = PopulationDynamics.RandomDeath(
        Rate(0.2),
        PopulationDynamicsProfile.Flat
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