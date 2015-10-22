package com.andbutso.jester

import scala.collection.mutable

object AllPlayers {
	type SortPredicate = Valueable => Double

	val byValue: SortPredicate = { -_.value }
	val byPoints: SortPredicate = { - _.projectedPoints }
	val bySalary: SortPredicate = { - _.salary }
	val defaultSort = byValue
}

case class LineupBuilder(
  players: AllPlayers,
  rosterRequirement: RosterRequirement,
  predicate: AllPlayers.SortPredicate = AllPlayers.defaultSort
) {
  import scala.language.implicitConversions

  case class RichPlayerSeq[T](players: Seq[T]) {
    def takePercent(percent: Int) = {
      val amount = Math.ceil(players.size * percent/100.0).toInt
      players.take(amount)
    }
  }

  implicit def playerSeqToRichPlayerSeq[T](playerSeq: Seq[T]): RichPlayerSeq[T] = {
    RichPlayerSeq(playerSeq)
  }

  val summariesByPositionSlot = rosterRequirement.slots.distinct.map { slot =>
    slot -> players.positionSummary(slot, rosterRequirement, predicate)
  }.toMap

  def foo = {
    val perPosition = mutable.Set[Set[PossiblePlayers]]()

    rosterRequirement.slots.distinct.foreach { slot =>
      val summary = summariesByPositionSlot(slot)
      val number = rosterRequirement.slots.count { _ == slot }
      println(s"$number slots for $slot")
      val applicable = applicablePlayers(slot).sortBy(predicate)
      println(s"${applicable.size} players for $slot")

      val targetPercent = summary.targetPercentage.toInt
      val targetted     = applicable.takePercent(targetPercent)
      val combos        = targetted.combinations(number).map {
        PossiblePlayers(rosterRequirement, _)
      }.toSeq.sortBy(predicate).toSet

      println(s"${combos.size} combos of ${targetted.size} ($targetPercent%)")
      val remainder = if (number == 1) {
        combos.take(Math.min(15, combos.size))
      } else {
//        val targettedPlayersWithoutRosters = mutable.Set(targetted.map { _.ref }: _*)
//
//        combos.takeWhile { pp =>
//          pp.players.foreach { pl =>
//            targettedPlayersWithoutRosters -= pl
//          }
//          targettedPlayersWithoutRosters.nonEmpty
//        }
        val amount = Math.min(15*number, combos.size)
        combos.take(amount)
      }

      println(s"Keeping ${remainder.size}")
      perPosition.add(remainder)
    }

    val estimatedMaxSize = perPosition.toSeq.map { _.size }.product
    println(s"Estimated max size: $estimatedMaxSize")
    val allCombined = mutable.Set(perPosition.head.toSeq: _*)

      val res = perPosition.tail.foldLeft(allCombined) { case (allPps, nextSetOfCombos) =>
      val cheapest = nextSetOfCombos.minBy { _.players.minBy { _.salary }.salary }.players.minBy { _.salary }.salary
      allPps.foreach { outer =>
        if (outer.isWithinBudget && outer.remainingBudget >= cheapest && !outer.isFull) {
          nextSetOfCombos.foreach { inner =>
            val possibility = inner + outer
            if (possibility.isWithinBudget) {
              allPps.add(possibility)
            }
          }
        }
      }

      allPps
    }
    println(s"raw result count: ${res.size}")
    val filtered = mutable.ArrayBuffer[PossiblePlayers]()
    res.foreach { pp =>
      if (pp.isFull && rosterRequirement.isValid(pp)) {
        filtered += pp
      }
    }
    Rosters(filtered)
  }

  def applicablePlayers(positionSlot: PositionSlot) = {
    players.players.filter { player =>
      positionSlot.canBeFilledBy(player)
    }
  }
}

case class AllPlayers(
	players: Seq[Player]
) {
	import scala.language.implicitConversions

  case class RichPlayerSeq[T](players: Seq[T]) {
    def takePercent(percent: Int) = {
      val amount = Math.ceil(players.size * percent/100.0).toInt
      players.take(amount)
    }
  }

	implicit def playerSeqToRichPlayerSeq[T](playerSeq: Seq[T]): RichPlayerSeq[T] = {
		RichPlayerSeq(playerSeq)
	}

  def positionSummary(
    positionSlot: PositionSlot,
    rosterRequirement: RosterRequirement,
    predicate: AllPlayers.SortPredicate = AllPlayers.defaultSort
  ) = {
    PositionSummary(players, positionSlot, rosterRequirement, predicate)
  }

	val byPosition = players.groupBy { _.position }

	lazy val possibleRBs = {
		filterDown(
			Position.RB,
			initialPercent = 30, // 30
			groupSize = 2
		)
	}

	lazy val possibleWRs = {
		filterDown(
			Position.WR,
			initialPercent = 35, // 35
			groupSize = 3,
			finalPercent = Some(7) // 7
		)
	}

	lazy val possibleRBsWRs = {
		filterDown(
			possibleRBs,
			possibleWRs,
			percentToKeep = 10 // 10
		)
	}

	lazy val possibleQBs = {
		filterDown(
			Position.QB,
			initialPercent = 40, // 40
			groupSize = 1
		)
	}

	lazy val possibleTEs = {
		filterDown(
			Position.TE,
			initialPercent = 20,
			groupSize = 1
		)
	}

	lazy val possibleFlex = {
		filterDown(
			Position.flexPositions,
			percentToKeep = 33 // 33
		)
	}

	lazy val possibleDEFFlex = {
		filterDown(
			possibleFlex,
			possibleDEFs,
			percentToKeep = 30
		)
	}

	lazy val possibleTEsFlex = {
		filterDown(
			possibleTEs,
			possibleFlex,
			percentToKeep = 10 // 10
		)
	}

	lazy val possibleQBsRBsWRs = {
		filterDown(
			possibleQBs,
			possibleRBsWRs,
			percentToKeep = 2 // 2
		)
	}

	lazy val possibleQBsRBsWRsTEsFlex = {
		filterDown(
			possibleQBsRBsWRs,
			possibleTEsFlex,
			percentToKeep = 5 // 5
		)
	}

	lazy val possibleDEFs = {
		filterDown(
			Set(Position.DEF, Position.DST),
			percentToKeep = 50
		)
	}

	lazy val cheapestDEF = {
		possibleDEFs.minBy { _.salary }.salary
	}

	lazy val bestRosters = {
		Rosters(
			filterDown(
				possibleQBsRBsWRsTEsFlex,
				possibleDEFs,
				percentToKeep = 100
			)
		)
	}

	def filterDown(
		position: Position.Value,
		initialPercent: Int,
		groupSize: Int,
		finalPercent: Option[Int] = None,
		sortPredicate: AllPlayers.SortPredicate = AllPlayers.defaultSort
	): Seq[PossiblePlayers] = {
		byPosition(position).sortBy(sortPredicate).takePercent(initialPercent).combinations(groupSize).map { possiblePlayers =>
			PossiblePlayers(NFL.rosterRequirements, possiblePlayers)
		}.toSeq.sortBy(sortPredicate).takePercent(if (groupSize == 1) 100 else finalPercent.getOrElse(initialPercent))
	}

	def filterDown(
		positions: Set[Position.Value],
		percentToKeep: Int
	): Seq[PossiblePlayers] = {
		positions.flatMap { position =>
			byPosition.get(position)
		}.flatten.toSeq.sortBy(AllPlayers.defaultSort).takePercent(percentToKeep).combinations(1).map { possiblePlayers =>
			PossiblePlayers(NFL.rosterRequirements, possiblePlayers)
		}.toSeq
	}

	def filterDown(
		possiblePlayerGroup1: Seq[PossiblePlayers],
		possiblePlayerGroup2: Seq[PossiblePlayers],
		percentToKeep: Int
	): Seq[PossiblePlayers] = {
		// val rosterCountsPerPlayer = mutable.Map[String, Int]()
		// val maxPercentOfRosters = 50
		// val hypotheticalMax = possiblePlayerGroup1.size * possiblePlayerGroup2
		val possibilities = mutable.Set[PossiblePlayers]()

		possiblePlayerGroup1.foreach { group1 =>
			if (group1.isWithinBudget && group1.remainingBudget >= cheapestDEF) {
				possiblePlayerGroup2.foreach { group2 =>
					val possibility = group1 + group2
					if (possibility.isWithinBudget) {
						possibilities.add(group1 + group2)
					}
				}
			}
		}

		possibilities.toSeq.sortBy(AllPlayers.defaultSort).takePercent(percentToKeep)
	}

	def apply(name: String): Seq[Player] = {
		players filter { player =>
			player.name.contains(name)
		}
	}

	def apply(names: Seq[String]): Seq[Player] = {
		names.flatMap { name =>
			this(name)
		}
	}

	def alternativesFor(
		player: Player,
		budget: Option[Int],
		isFlex: Option[Boolean]
	): Seq[Player] = {
		val includeAllFlexPositions = isFlex.getOrElse(Position.flexPositions.contains(player.position))

		players.filter { replacementCandidate =>
			if (player.id != replacementCandidate.id && replacementCandidate.salary <= budget.getOrElse(player.salary)) {
				if (includeAllFlexPositions) {
					replacementCandidate.isFlex
				} else {
					player.position == replacementCandidate.position
				}
			} else {
				false
			}
		}.sortBy(AllPlayers.byPoints)
	}

	def alternativesFor(
		name: String,
		budget: Option[Int] = None,
		isFlex: Option[Boolean] = None
	): Seq[Player] = {
		this(name).headOption.map { player =>
			alternativesFor(player, budget, isFlex)
		}.getOrElse(Seq.empty)
	}
}
