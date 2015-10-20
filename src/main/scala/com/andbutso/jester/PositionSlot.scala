package com.andbutso.jester

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
		val QB = PositionSlot(Position.QB)
		val RB = PositionSlot(Position.RB)
		val WR = PositionSlot(Position.WR)
		val FLEX = PositionSlot(Set(Position.RB, Position.WR))
	}
}

case class PositionSlot(accepts: Set[Position.Value], player: Option[Player] = None) {
	def canFillSlot(position: Position.Value) = {
		accepts.contains(position)
	}
}
