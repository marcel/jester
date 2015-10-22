package com.andbutso.jester

case class Player(
	name: String,
	salary: Int,
	projectedPoints: Double,
	position: Position.Value,
	id: String,
	injuryStatus: String
) extends Valueable {
	val ref = PlayerRef(id)

	val value = {
		MathUtil.truncate(projectedPoints / salary * projectedPoints * 100.0)
	}

	def hasValue = projectedPoints > 0
	def isFlex = Position.flexPositions.contains(position)
}
