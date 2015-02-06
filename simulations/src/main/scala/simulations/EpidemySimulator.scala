package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  def randomSelect[T](xs: List[T]): T = xs(randomBelow(xs.size))

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
    val maxDaysToWaitBeforeMove = 5

    val airTravel = false
    val reducedMobility = false
    val mobilityFactorHealthy = 2
    val mobilityFactorInfected = 4
    val vaccination = false
    val vaccinationRate = 0.05
  }

  import SimConfig._

  val persons: List[Person] = mkPeople

  def mkPeople(): List[Person] = {
    val nInfected = (prevalenceRate * population).round.toInt
    val nVaccinated = (vaccinationRate * population).round.toInt
    val people = (1 to population) map (new Person(_))
    if (vaccination) {
      for (person ← people.take(nVaccinated))
        person.vaccinated = true
    }
    for (person ← people.filterNot(_.vaccinated).take(nInfected))
      person.infected = true
    people.toList
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var dead = false
    var immune = false
    var vaccinated = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    def computeDaysLeftToMove() = {
      var factor = 1
      val nDays = randomBelow(maxDaysToWaitBeforeMove)
      if (reducedMobility) {
        if (infected)
          factor = mobilityFactorInfected
        else
          factor = mobilityFactorHealthy
      }
      factor * nDays
    }

    var daysLeftToMove = computeDaysLeftToMove
    var incubationPeriod: Int = 0

    def isReadyToMove: Boolean = {
      if (daysLeftToMove == 0) {
        daysLeftToMove = computeDaysLeftToMove
        true
      } else {
        daysLeftToMove -= 1
        false
      }
    }

    def isHealthy(): Boolean =
      (!infected & !sick & !dead) | vaccinated

    def isVisiblyInfectious(): Boolean = sick | dead

    def probablyTransitionToInfected: Unit = {
      assert(!dead)
      infected = !vaccinated & (randomBelow(population) <= (0.4 * population))
    }

    def transitionToHealthy: Unit = {
      assert(!dead)
      infected = false
      sick = false
      immune = false
      incubationPeriod = 0
    }

    def transitionToImmune: Unit = {
      assert(!dead)
      assert(infected)
      sick = false
      immune = true
    }

    def probablyTransitionToDeath: Unit = {
      assert(!dead)
      dead = (randomBelow(population) <= (0.25 * population))
    }

    def updatePosition(pos: (Int, Int)): Unit = {
      row = pos._1
      col = pos._2
    }

    override def toString =
      Map(
        "infected" -> infected,
        "sick" -> sick,
        "immune" -> immune,
        "dead" -> dead,
        "pos" -> (row, col),
        "incubation period" -> incubationPeriod,
        "days left to move" -> daysLeftToMove).toString
  }

  def nextDay(): Unit = {
    updateRooms()
    move(persons filterNot (_.dead))
  }

  def updateRooms(): Unit = {
    for {
      r ← 0 until roomRows
      c ← 0 until roomColumns
    } {
      val peopleInRoom = peopleAtPos((r, c))
      val (healthy, unhealthy) = peopleInRoom partition (_.isHealthy)
      if (unhealthy.size > 0) {
        for (p ← healthy) p.probablyTransitionToInfected
      }
      for (unhealthyPerson ← unhealthy if !unhealthyPerson.dead)
        updateUnhealthyStatus(unhealthyPerson)
    }
  }

  def updateUnhealthyStatus(person: Person): Unit = {
    assert(!person.isHealthy)
    person.incubationPeriod += 1
    val ip = person.incubationPeriod
    if (ip >= 18)
      person.transitionToHealthy
    else {
      if (ip >= 16)
        person.transitionToImmune
      else if (ip >= 14)
        person.probablyTransitionToDeath
      else if (ip >= 6)
        person.sick = true
    }
  }

  def move(people: List[Person]): Unit = {
    for (person ← people) {
      if (person.isReadyToMove) {
        val newPoss = newValidPositions((person.row, person.col))
        if (newPoss.size > 0) {
          val newPos = randomSelect(newPoss)
          person updatePosition newPos
        }
      }
    }
  }

  def newValidPositions(pos: (Int, Int)): List[(Int, Int)] = {
    if (canAirTravel()) {
      List((randomBelow(roomRows), randomBelow(roomColumns)))
    } else {
      nonVisiblyInfectedNeighbors(pos)
    }
  }

  def canAirTravel(): Boolean =
    airTravel & randomBelow(population) <= 0.1 * population

  def nonVisiblyInfectedNeighbors(pos: (Int, Int)) =
    neighbors(pos) filterNot containsVisiblyInfected

  def neighbors(pos: (Int, Int)): List[(Int, Int)] = {
    val diff = List((-1, 0), (0, 1), (1, 1), (0, -1))
    for ((i, j) ← diff)
      yield (
      (i + pos._1 + roomRows) % roomRows,
      (j + pos._2 + roomColumns) % roomColumns)
  }

  def containsVisiblyInfected(pos: (Int, Int)): Boolean =
    (peopleAtPos(pos) filter (_.isVisiblyInfectious)).size > 0

  def peopleAtPos(pos: (Int, Int)) =
    persons filter (p ⇒ (p.row == pos._1 & p.col == pos._2))

  def everyDay(): Unit = {
    nextDay
    afterDelay(1)(everyDay())
  }
  afterDelay(0)(everyDay())
}

