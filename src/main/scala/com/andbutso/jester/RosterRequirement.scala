package com.andbutso.jester

object RosterRequirement {

}

case class RosterRequirement(slots: Seq[PositionSlot]) {
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