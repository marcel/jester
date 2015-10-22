package com.andbutso.jester

import scala.collection.mutable

object PossiblePlayers {
	val budget = 50000
	val numberOfPositionsInRoster = 9

	val minimumSlotsPerPosition = Map(
		Position.TE -> 1,
		Position.RB -> 2,
		Position.WR -> 3
	)

	def apply(rosterRequirement: RosterRequirement, players: Seq[Player]): PossiblePlayers = {
		PossiblePlayers(rosterRequirement, players.map { _.ref }.toSet)
	}
}

case class RosterOptimizer(
  possiblePlayers: PossiblePlayers,
  allPlayers: AllPlayers = PlayerData.all
) {
  def alternativesFor(name: String) = {
    allPlayers(name).headOption.map { player =>
      val isFlex: () => Boolean = { () =>
        val remainingPlayers = possiblePlayers - player.ref

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
        budget = Some(possiblePlayers.remainingBudget + player.salary),
        isFlex = Some(isFlex())
      )
    }.getOrElse(Seq.empty[Player])
  }

  def improveOn(name: String, withReplacementRanked: Int = 0) = {
    possiblePlayers(name).headOption.map { playerToImproveOn =>
      val alternatives = alternativesFor(name).filterNot { alternativePlayer =>
        possiblePlayers.players.contains(alternativePlayer.ref)
      }

      val index = Math.min(withReplacementRanked, alternatives.size - 1)
      if (index >= 0) {
        possiblePlayers.swap(playerToImproveOn, alternatives(index).ref)
      } else {
        possiblePlayers
      }
    }.getOrElse(possiblePlayers)
  }

  def improvements(numberPerPlayer: Int = 1) = {
    possiblePlayers.players.flatMap { playerToImproveOn =>
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
      possiblePlayers
    }
  }

  def optimize: PossiblePlayers = {
    val improvement = improve()
    if (possiblePlayers == improvement || improvement.projectedPoints <= possiblePlayers.projectedPoints) {
      improvement
    } else {
      improvement.optimize
    }
  }
}

case class PossiblePlayers(
  rosterRequirement: RosterRequirement,
  players: Set[PlayerRef]
) extends Valueable {
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

  lazy val (filledSlots, emptySlots) = {
    val alreadyAssigned = mutable.Set[PlayerRef]()

    rosterRequirement.slots.map { slot =>
      players.foldLeft(slot) { case (s, player) =>
        if (s.isEmpty && s.canBeFilledBy(player.player) && !alreadyAssigned.contains(player)) {
          alreadyAssigned.add(player)
          s.fillWith(player.player)
        } else {
          s
        }
      }
    }.partition { _.isFilled }
  }

	lazy val byPosition = {
		players.groupBy { _.position }
	}

  def optimizerWithPlayers(allPlayers: AllPlayers = PlayerData.all) = {
    RosterOptimizer(this, allPlayers)
  }

  def optimizer = {
    RosterOptimizer(this)
  }

  def alternativesFor(name: String) = {
    optimizer.alternativesFor(name)
  }

  def improveOn(name: String, withReplacementRanked: Int = 0) = {
    optimizer.improveOn(name, withReplacementRanked)
  }

  def improvements(numberPerPlayer: Int = 1) = {
    optimizer.improvements(numberPerPlayer)
  }

	def improve(withImprovementRanked: Int = 0) = {
    optimizer.improve(withImprovementRanked)
	}

   def optimize: PossiblePlayers = {
    optimizer.optimize
	}

	def swap(oldPlayer: PlayerRef, newPlayer: PlayerRef) = {
		copy(players = players - oldPlayer + newPlayer)
	}

  def canSwap(oldPlayer: PlayerRef, newPlayer: PlayerRef) = {
    swap(oldPlayer, newPlayer).isWithinBudget
  }

  def swapIn(newPlayer: PlayerRef) = {

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
    if (isFull) {
      this
    } else {
      copy(players = players ++ otherPossiblePlayers.players)
//      otherPossiblePlayers.players.foldLeft(this) { case (pp, player) =>
//        if (pp.emptySlots.exists {_.canBeFilledBy(player.player)}) {
//          pp.copy(players = players + player)
//        } else {
//          pp
//        }
//      }
    }
	}

	def -(player: PlayerRef) = {
		copy(players = players - player)
	}

//	def isFull = {
//		players.size == PossiblePlayers.numberOfPositionsInRoster
//	}

  def isFull = {
    !players.isEmpty && emptySlots.isEmpty
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

  def toCSV = {
    filledSlots.flatMap { _.player }.map { _.name }.mkString(",")
  }

	val projectedPoints = MathUtil.truncate(players.toSeq.map { _.projectedPoints }.sum)
	val value  = MathUtil.truncate(players.toSeq.map { _.value }.sum)
	val salary = players.toSeq.map { _.salary }.sum
}