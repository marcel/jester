package com.andbutso.jester

import scala.collection.mutable

case class PositionSummary(
  players: Seq[Player],
  positionSlot: PositionSlot,
  rosterRequirement: RosterRequirement,
  predicate: AllPlayers.SortPredicate = AllPlayers.defaultSort
) {
  import MathUtil.{percent, truncate}

  def targetPercentage = {
//    if (medianPercentOfMax > 50) {
//      percentOfPlayersFiftyPercentGreaterThanMedian
//    } else {
//      10.0
//    }
    percentOfPlayersFiftyPercentGreaterThanAvg
  }

  val applicablePlayers = players.filter { player =>
    positionSlot.canBeFilledBy(player.position)
  }

  val rosterSlots = rosterRequirement.slots

  val percentOfTotalPlayers = percent(applicablePlayers.size / players.size.toFloat)
  val percentOfRoster = percent(rosterSlots.count {
    _ == positionSlot
  }.toFloat / rosterSlots.size)

  val sorted = applicablePlayers.sortBy(predicate)
  val max = truncate(Math.abs(predicate(sorted.head)))
  val top4RunnersUp = sorted.tail.take(4).map { player => truncate(Math.abs(predicate(player))) }

  val topTenPercent = sorted.take(Math.round(sorted.size * 0.1).toInt)
  val medianTopTenPercent = truncate(Math.abs(predicate(topTenPercent.take(Math.round(topTenPercent.size / 2)).last)))
  val sumTopTenPercent = topTenPercent.map { player =>
    Math.abs(predicate(player))
  }.sum
  val avgTopTenPercent = truncate(sumTopTenPercent / topTenPercent.size.toFloat)

  val min = truncate(Math.abs(predicate(sorted.last)))
  val median = truncate(Math.abs(predicate(sorted.take(Math.round(sorted.size / 2)).last)))
  val medianPercentOfMax = percent(median / max.toFloat)

  val sum = applicablePlayers.map { player =>
    Math.abs(predicate(player))
  }.sum

  val average = truncate(sum.toFloat / applicablePlayers.size)
  val averagePercentOfMax = percent(average / max.toFloat)

  val numberOfPlayersWithinHalfOfMax = applicablePlayers.count { player =>
    Math.abs(predicate(player)) >= max / 2
  }
  val percentOfPlayersWithinHalfOfMax = percent(numberOfPlayersWithinHalfOfMax / applicablePlayers.size.toFloat)

  val numberOfPlayersWithinThreeQuartersOfMax = applicablePlayers.count { player =>
    Math.abs(predicate(player)) >= max * 0.75
  }
  val percentOfPlayersWithinThreeQuartersOfMax = percent(
    numberOfPlayersWithinThreeQuartersOfMax / applicablePlayers.size.toFloat
  )

  val fiftyPercentOfMedian = truncate(median * 1.5)
  val numberOfPlayersFiftyPercentGreaterThanMedian = applicablePlayers.count { player =>
    fiftyPercentOfMedian <= Math.abs(predicate(player))
  }

  val percentOfPlayersFiftyPercentGreaterThanMedian = percent(
    numberOfPlayersFiftyPercentGreaterThanMedian / applicablePlayers.size.toFloat
  )

  val fiftyPercentOfAvg = truncate(average * 1.5)
  val numberOfPlayersFiftyPercentGreaterThanAvg = applicablePlayers.count { player =>
    fiftyPercentOfAvg <= Math.abs(predicate(player))
  }

  val percentOfPlayersFiftyPercentGreaterThanAvg = percent(
    numberOfPlayersFiftyPercentGreaterThanAvg / applicablePlayers.size.toFloat
  )

  def summary = {
    val lines = new mutable.StringBuilder
    val positions = positionSlot.accepts.mkString("/")
    val runnersUp = top4RunnersUp.mkString("/")
    lines.append(s"$positions\n")
    lines.append("-" * positions.length + "\n")
    lines.append(s"${applicablePlayers.size} of ${players.size} ($percentOfTotalPlayers%)\n")
    lines.append(s"$percentOfRoster% of roster\n")
    lines.append(s"Max: $max ($runnersUp)\n")
    lines.append(s"90p: $medianTopTenPercent (50p) $avgTopTenPercent (avg) ${topTenPercent.size} (total)\n")
    lines.append(s"50p: $median ($medianPercentOfMax% of max)\n")
    lines.append(s"Avg: $average ($averagePercentOfMax% of max) \n")
    lines.append(s"Min: $min\n")
    lines.append(s"W/in 50% of max: $numberOfPlayersWithinHalfOfMax ($percentOfPlayersWithinHalfOfMax%)\n")
    lines.append(s"W/in 75% of max: $numberOfPlayersWithinThreeQuartersOfMax ($percentOfPlayersWithinThreeQuartersOfMax%)\n")
    lines.append(s"150% of 50p: $fiftyPercentOfMedian\n")
    lines.append(s"150% > 50p: $numberOfPlayersFiftyPercentGreaterThanMedian ($percentOfPlayersFiftyPercentGreaterThanMedian%)\n")
    lines.append(s"150% of avg: $fiftyPercentOfAvg \n")
    lines.append(s"150% > avg: $numberOfPlayersFiftyPercentGreaterThanAvg ($percentOfPlayersFiftyPercentGreaterThanAvg%)\n")

    lines.result()
  }
}
