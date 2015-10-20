package com.andbutso.jester

object Position extends Enumeration {
	// Football
	val QB, RB, WR, TE, DEF, DST = Value
	val flexPositions = Set(RB, WR, TE)
}
