package com.andbutso.jester

import scala.collection.mutable

object AllPlayers {
	type SortPredicate = Valueable => Double

	val byValue: SortPredicate = { -_.value }
	val byPoints: SortPredicate = { - _.projectedPoints }
	val bySalary: SortPredicate = { - _.salary }
	val defaultSort = byValue
}

case class AllPlayers(
	players: Seq[Player]
) {
	import scala.language.implicitConversions
	case class RichPlayerSeq[T](players: Seq[T]) {
		def takePercent(percent: Int) = {
			val amount = (players.size * percent/100.0).toInt
			players.take(amount)
		}
	}

	implicit def playerSeqToRichPlayerSeq[T](playerSeq: Seq[T]) = {
		RichPlayerSeq(playerSeq)
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
			percentToKeep = 10 // 5
		)
	}
	// lazy val possibleQBsRBsWRsTEsFlex = {
	// 	filterDown(
	// 		possibleQBsRBsWRs,
	// 		possibleDEFFlex,
	// 		percentToKeep = 10 // 5
	// 	)
	// }

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
			PossiblePlayers(possiblePlayers)
		}.toSeq.sortBy(sortPredicate).takePercent(if (groupSize == 1) 100 else finalPercent.getOrElse(initialPercent))
	}

	def filterDown(
		positions: Set[Position.Value],
		percentToKeep: Int
	): Seq[PossiblePlayers] = {
		positions.flatMap { position =>
			byPosition.get(position)
		}.flatten.toSeq.sortBy(AllPlayers.defaultSort).takePercent(percentToKeep).combinations(1).map { possiblePlayers =>
			PossiblePlayers(possiblePlayers)
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
		}.toSeq.sortBy(AllPlayers.byPoints)
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
