package com.andbutso.jester

import scala.collection.mutable

case class Rosters(rosters: Seq[PossiblePlayers]) {
	def byPoints = {
		rosters.sortBy(AllPlayers.byPoints)
	}

	def byValue = {
		rosters.sortBy(AllPlayers.byValue)
	}

	def bySalary = {
		rosters.sortBy(AllPlayers.bySalary)
	}

	def withPlayer(name: String) = {
		Rosters(
			rosters.filter { roster =>
				roster.players.exists { _.name.contains(name) }
			}
		)
	}

	def excludingPlayer(name: String) = {
		Rosters(
			rosters.filterNot { roster =>
				roster.players.exists { _.name.contains(name) }
			}
		)
	}

	def excludingPlayers(names: Seq[String]) = {
		names.foldLeft(this) { case (remaining, name) =>
			remaining.excludingPlayer(name)
		}
	}

	def rosterCountsPerPlayerByPosition(
		position: Position.Value = Position.QB,
		threshold: Int = 1
	) = {
		val countsByPlayer = mutable.Map[String, Int]()

		rosters.foreach { roster =>
			roster.byPosition.get(position).foreach { players =>
				players.foreach { player =>
					val currentCount = countsByPlayer.getOrElseUpdate(player.name, 0)
					countsByPlayer(player.name) = currentCount + 1
				}
			}
		}

		countsByPlayer.filter { case (name, count) =>
			count >= threshold
		}.toSeq.sortBy { case (name, count) => count }
	}

	def rosterPerPlayerByPosition(
		position: Position.Value = Position.QB,
		threshold: Int = 1,
		limitTo: Int = 100
	) = {
		val rostersPerPlayer = mutable.Map[String, Set[PossiblePlayers]]()

		rosters.foreach { roster =>
			roster.byPosition.get(position).foreach { players =>
				players.foreach { player =>
					val currentRosters = rostersPerPlayer.getOrElseUpdate(player.name, Set.empty[PossiblePlayers])
					rostersPerPlayer(player.name) = currentRosters + roster
				}
			}
		}

		rostersPerPlayer.filter { case (name, rosters) =>
			rosters.size >= threshold
		}.toSeq.sortBy { case (name, rosters) =>
			rosters.size
		}.map { case (name, rosters) =>
			(name, Rosters(Rosters(rosters.toSeq).byPoints.take(limitTo)))
		}.toMap
	}
}
