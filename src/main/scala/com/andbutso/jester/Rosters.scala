package com.andbutso.jester


import scala.annotation.tailrec
import scala.collection.mutable

case class PlayerExposureAdjustment(name: String, rosters: Rosters) {
  val player             = rosters.allPlayers(name).head
  val currentRosterCount = rosters.rosterCountsPerPlayer(player.name)
  val currentExposure    = Math.round(currentRosterCount / rosters.rosters.size * 100)

  val (rostersWith, rostersWithout) = util.Random.shuffle(rosters.rosters).partition { roster =>
    roster.players.contains(player.ref)
  }

  def adjustTo(targetPercent: Int) = {
    val targetCount = Math.round(rosters.rosters.size * (targetPercent.toFloat / 100))
    if (targetPercent < currentExposure) {
      adjustDownTo(targetCount)
    } else {
      adjustUpTo(targetCount)
    }
  }

  def adjustDownTo(targetCount: Int) = {
    val numberOfRostersToRemoveFrom = currentRosterCount - targetCount

    val toRemoveFrom = rostersWith.slice(0, numberOfRostersToRemoveFrom)
    val toKeep       = rostersWith.slice(numberOfRostersToRemoveFrom, rostersWith.size)

    val rostersWithPlayerRemoved = toRemoveFrom.map { roster =>
      roster.optimizerWithPlayers(rosters.allPlayers).improveOn(player.name)
    }

    Rosters(rostersWithPlayerRemoved ++ toKeep ++ rostersWithout)
  }

  def adjustUpTo(targetCount: Int) = {
    val numberOfRostersToAddTo = targetCount - currentRosterCount
    val toAddTo     = rostersWithout.slice(0, numberOfRostersToAddTo)
    val toLeaveAsIs = rostersWithout.slice(numberOfRostersToAddTo, rostersWithout.size)

    val rostersWithPlayerAdded = toAddTo.map { roster =>
      val optimizer = roster.optimizerWithPlayers(rosters.allPlayers)
      val alternatives = optimizer.alternativesFor(player.name).toSet

      roster.players.find { potentialPlayerToSwapWith =>
        alternatives.contains(potentialPlayerToSwapWith.player) &&
          roster.canSwap(potentialPlayerToSwapWith, player.ref)
      }.map { playerToSwapWith =>
        roster.swap(playerToSwapWith, player.ref)
      }.getOrElse(roster)
    }

    Rosters(rostersWithPlayerAdded ++ toLeaveAsIs ++ rostersWith)
  }
}

case class Rosters(rosters: Seq[PossiblePlayers]) {
  lazy val allPlayers = {
    AllPlayers(rosters.flatMap { _.players.map { _.player } }.distinct)
  }

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

	def sample(percent: Int) = {
    val targetSampleSize = Math.round(rosters.size * (percent.toFloat / 100))
		Rosters(util.Random.shuffle(rosters).take(targetSampleSize))
	}

  @tailrec
  final def changePlayerExposure(name: String, targetPercent: Int, attempts: Int = 0): Rosters = {
    if (attempts > 5) {
      this
    } else {
      val result = PlayerExposureAdjustment(name, this).adjustTo(targetPercent)
      val adjustment = PlayerExposureAdjustment(name, result)
      if (adjustment.currentExposure != targetPercent) {
        result.changePlayerExposure(name, targetPercent, attempts + 1)
      } else {
        result
      }
    }
  }

  // Point projection bucket counts
  // r26.byPoints.map { _.projectedPoints.toInt }.groupBy { num => (num/10.0).toInt}.map { case (bucket, scores) => bucket*10 -> scores.size }.toSeq.sortBy { case (b, c)=> -b } foreach println

  def rosterCountsForAllPositions = {
    val positions = rosters.head.rosterRequirement.slots.flatMap { _.accepts }.distinct
    positions.map { position =>
      position -> rosterCountsPerPlayerByPosition(position)
    }.toMap
  }

  def rosterCountsPerPlayer = {
    val countsByPlayer = mutable.Map[String, Int]()

    rosters.foreach { roster =>
      roster.players.foreach { player =>
        val currentCount = countsByPlayer.getOrElseUpdate(player.name, 0)
        countsByPlayer(player.name) = currentCount + 1
      }
    }

    countsByPlayer
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
		}.toSeq.sortBy { case (name, count) => -count }
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
