// #!/bin/sh
// exec scala "$0" "$@"
// !#

import scala.collection.mutable
import scala.annotation.tailrec

object PlayerData {
	val path = "/Users/marcel/Desktop/draftkings-players-10-19-2015.csv"
	// val path = "/Users/marcel/Desktop/ffn-players-10-13-2015.csv"
	val lines = io.Source.fromFile(path).getLines.toArray.tail
	val data = lines map { _.split(",") }
	lazy val all = load
	lazy val allById = all.players.map { player => player.id -> player }.toMap
	
	def load = {
		AllPlayers(
			data collect { 
				case (Array(playerName, salary, projectedPoints, position, id)) if !playerName.startsWith("#") =>
					Player(playerName, salary.toInt, projectedPoints.toFloat, Position.withName(position), id)
			} filter { _.hasValue }
		)	
	}
}

object MathUtil {
	def truncate(d: Double) = {
		Math.round(d * 100.0) / 100.0
	}
}

object Position extends Enumeration {
	// Football
	val QB, RB, WR, TE, DEF, DST = Value
	val flexPositions = Set(RB, WR, TE)
}

case class Player(
	name: String,
	salary: Int,
	projectedPoints: Double,
	position: Position.Value,
	id: String
) extends Valueable {
	val ref = PlayerRef(id)
	
	val value = {
		MathUtil.truncate(projectedPoints / salary * projectedPoints * 100.0)
	}
	
	def hasValue = projectedPoints != 0 || value > 0
	def isFlex = Position.flexPositions.contains(position)
}

case class PlayerRef(id: String) extends Valueable {
	def player = PlayerData.allById(id)
	def value  = player.value
	def salary = player.salary
	def name   = player.name
	
	def position = player.position
	
	def projectedPoints = player.projectedPoints
	override def toString = s"Ref(${player.toString})"
}

trait Valueable {
	def value: Double
	def salary: Int
	def projectedPoints: Double
}

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

object PositionSlot {
	def apply(position: Position.Value): PositionSlot = {
		PositionSlot(Set(position))
	}
	
	object NFL {
		val QB   = PositionSlot(Position.QB)
		val WR   = PositionSlot(Position.WR)
		val RB   = PositionSlot(Position.RB)
		val TE   = PositionSlot(Position.TE)
		val FLEX = PositionSlot(Set(Position.WR, Position.RB, Position.TE))
		val DST  = PositionSlot(Set(Position.DST, Position.DEF))
	}
	
	object CFB {
		val QB = PositionSlot(PositionSlot.QB)
		val RB = PositionSlot(PositionSlot.RB)
		val WR = PositionSlot(PositionSlot.WR)
		val FLEX = PositionSlot(Set(Position.RB, Position.WR))
	}
}

object RosterRequirement {
	
}

object NFL {
	import PositionSlot.{NFL => slot}
	
	val rosterRequirements = RosterRequirement(
		Seq(
			slot.QB,
			slot.WR,
			slot.WR,
			slot.WR,
			slot.RB,
			slot.RB,
			slot.TE,
			slot.FLEX,
			slot.DST
		)
	)
}

object CFB {
	import PositionSlot.{CFB => slot}
	val rosterRequirements = RosterRequirement(
		Seq(
			slot.QB,
			slot.QB,
			slot.RB,
			slot.RB,
			slot.WR,
			slot.WR,
			slot.WR,
			slot.FLEX,
			slot.FLEX
		)
	)
}



case class RosterRequirement(slots: Seq[PositionSlot])

case class PositionSlot(accepts: Set[Position.Value], player: Option[Player] = None) {
	def canFillSlot(position: Position.Value) = {
		accepts.contains(position)
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
	
	val projectedPoints = MathUtil.truncate(players.toSeq.map { _.projectedPoints }.sum)
	val value  = MathUtil.truncate(players.toSeq.map { _.value }.sum)
	val salary = players.toSeq.map { _.salary }.sum
}