package com.andbutso.jester

import scala.annotation.tailrec
import scala.collection.mutable

object PossiblePlayers {
	val budget = 50000
	val numberOfPositionsInRoster = 9

	val minimumSlotsPerPosition = Map(
		Position.TE -> 1,
		Position.RB -> 2,
		Position.WR -> 3
	)

	def apply(players: Seq[Player]): PossiblePlayers = {
		PossiblePlayers(players.map { _.ref }.toSet)
	}
}

case class PossiblePlayers(players: Set[PlayerRef]) extends Valueable {
	def summary = {
		val lines = new mutable.StringBuilder

		lines.append(s"Points: ${projectedPoints}\n")
		lines.append(s"Cost: ${salary}\n")
		lines.append("\n")

		val longestNameLength = players.maxBy { _.name.size }.name.size
		players.toSeq.sortBy(AllPlayers.byValue).foreach { player =>
			val paddedName = player.name.split("").reverse.padTo(longestNameLength, " ").reverse.mkString
			val line = s"$paddedName: ${MathUtil.truncate(player.projectedPoints)} / ${player.salary} ${player.position}\n"
			lines.append(line)
		}

		lines.result
	}

	lazy val byPosition = {
		players.groupBy { _.position }
	}

	def alternativesFor(name: String) = {
		val allPlayers = PlayerData.all

		allPlayers(name).headOption.map { player =>
			val isFlex: () => Boolean = { () =>
				val remainingPlayers = this - player.ref

				player.isFlex && {
					PossiblePlayers.minimumSlotsPerPosition.forall { case (position, minimum) =>
						remainingPlayers.byPosition.get(position).exists { playersAtPosition =>
							playersAtPosition.size == minimum
						}
					}
				}
			}

			allPlayers.alternativesFor(
				player,
				budget = Some(remainingBudget + player.salary),
				isFlex = Some(isFlex())
			)
		}.getOrElse(Seq.empty[Player])
	}

	def improveOn(name: String, withReplacementRanked: Int = 0) = {
		this(name).headOption.map { playerToImproveOn =>
			val alternatives = alternativesFor(name).filterNot { alternativePlayer =>
				players.contains(alternativePlayer.ref)
			}

			val index = Math.min(withReplacementRanked, alternatives.size - 1)
			if (index >= 0) {
				swap(playerToImproveOn, alternatives(index).ref)
			} else {
				this
			}
		}.getOrElse(this)
	}

	def improvements(numberPerPlayer: Int = 1) = {
		players.flatMap { playerToImproveOn =>
			0.until(numberPerPlayer).map { number =>
				improveOn(playerToImproveOn.name, number)
			}
		}.toSeq.distinct.sortBy(AllPlayers.byPoints)
	}

	def improve(withImprovementRanked: Int = 0) = {
		val improvementOptions = improvements()

		val index = Math.min(withImprovementRanked, improvementOptions.size - 1)

		if (index >= 0) {
			improvementOptions(index)
		} else {
			this
		}
	}

	@tailrec
	final def optimize: PossiblePlayers = {
		val improvement = improve()
		if (this == improvement || improvement.projectedPoints < projectedPoints) {
			this
		} else {
			improvement.optimize
		}
	}

	def swap(oldPlayer: PlayerRef, newPlayer: PlayerRef) = {
		copy(players = players - oldPlayer + newPlayer)
	}

	def distance(otherPossiblePlayers: PossiblePlayers) = {
		players.diff(otherPossiblePlayers.players).size
	}

	def apply(name: String) = {
		players.filter { player =>
			player.name.contains(name)
		}
	}

	def +(otherPossiblePlayers: PossiblePlayers) = {
		copy(players = players ++ otherPossiblePlayers.players)
	}

	def -(player: PlayerRef) = {
		copy(players = players - player)
	}

	def isFull = {
		players.size == PossiblePlayers.numberOfPositionsInRoster
	}

	def remainingBudget = {
		PossiblePlayers.budget - salary
	}

	def isWithinBudget = {
		remainingBudget >= 0
	}

	def capSpaceValue = {
		projectedPoints * Math.log10(remainingBudget)
	}

  def toCSV(rosterRequirement: RosterRequirement) = {
    players.foldLeft(rosterRequirement) { case (rr, player) =>

      rr
    }
  }

	val projectedPoints = MathUtil.truncate(players.toSeq.map { _.projectedPoints }.sum)
	val value  = MathUtil.truncate(players.toSeq.map { _.value }.sum)
	val salary = players.toSeq.map { _.salary }.sum
}