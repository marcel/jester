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
}
