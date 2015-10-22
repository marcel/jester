package com.andbutso.jester

object RosterRequirement {

}

case class RosterRequirement(slots: Seq[PositionSlot]) {
  lazy val players = slots.flatMap { _.player }.toSet

  def +(player: Player) = {
    if (!players.contains(player)) {

    }
  }

  def numberOfSlotsForPosition(position: Position.Value) = {
    slotsForPosition(position).size
  }

  def percentOfRoster(position: Position.Value) = {
    numberOfSlotsForPosition(position) / slots.size
  }

  def slotsForPosition(position: Position.Value) = {
    slots.filter { _.canBeFilledBy(position) }
  }

  def requiredForPosition(position: Position.Value) = {
    slotsForPosition(position).filterNot { _.isFlex }
  }

  def flexForPosition(position: Position.Value) = {
    slotsForPosition(position).filter { _.isFlex }
  }

  def isValid(possiblePlayers: PossiblePlayers) = {
    val byPosition = possiblePlayers.byPosition
    possiblePlayers.players.size == slots.size && byPosition.forall { case (position, players) =>
      players.size >= requiredForPosition(position).size && players.size <= numberOfSlotsForPosition(position)
    }
  }

  def isFull(possiblePlayers: PossiblePlayers) = {
    possiblePlayers.players.size == slots.size
  }
}

object NFL {
  import PositionSlot.{NFL => slot}

  val rosterRequirements = RosterRequirement(
    Seq(
      slot.QB,
      slot.RB,
      slot.RB,
      slot.WR,
      slot.WR,
      slot.WR,
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

object NBA {
  import PositionSlot.{NBA => slot}
  val rosterRequirements = RosterRequirement(
    Seq(
      slot.PG,
      slot.SG,
      slot.SF,
      slot.PF,
      slot.C,
      slot.G,
      slot.F,
      slot.UTIL
    )
  )
}

object CBB {
  import PositionSlot.{CBB => slot}
  val rosterRequirements = RosterRequirement(
    Seq(
      slot.G,
      slot.G,
      slot.G,
      slot.F,
      slot.F,
      slot.F,
      slot.UTIL,
      slot.UTIL
    )
  )
}

object SOC {
  import PositionSlot.{SOC => slot}
  val rosterRequirements = RosterRequirement(
    Seq(
      slot.GK,
      slot.D,
      slot.D,
      slot.M,
      slot.M,
      slot.F,
      slot.F,
      slot.FLEX
    )
  )
}

object NHL {
  import PositionSlot.{NHL => slot}

  val rosterRequirements = RosterRequirement(
    Seq(
      slot.W,
      slot.W,
      slot.W,
      slot.C,
      slot.C,
      slot.D,
      slot.D,
      slot.UTIL,
      slot.G
    )
  )
}