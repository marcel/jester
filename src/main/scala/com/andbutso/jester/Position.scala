package com.andbutso.jester

object Position extends Enumeration {
	// Football
	val QB, RB, WR, TE, DEF, DST = Value
	val flexPositions = Set(RB, WR, TE)

	// Basketball
  val PG, SG, SF, PF, C, G, F = Value

	// Hockey
  val RW, LW, D = Value

  // Soccer
  val GK, M = Value

  // MLB
  val P, SS, OF = Value
  val SB   = Value("2B")
  val TB   = Value("3B")
  val FBDH = Value("1B/DH")

  class PositionValue(position: Value) {
    def slot = PositionSlot(position)
  }

  implicit def valueToPositionValue(position: Value): PositionValue = {
    new PositionValue(position)
  }
}
