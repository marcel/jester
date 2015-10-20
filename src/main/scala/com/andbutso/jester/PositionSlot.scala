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

  object NBA {
    val PG = PositionSlot(Position.PG)
    val SG = PositionSlot(Position.SG)
    val SF = PositionSlot(Position.SF)
    val PF = PositionSlot(Position.PF)
    val C  = PositionSlot(Position.C)
    val G  = PositionSlot(Set(Position.PG, Position.SG))
    val F  = PositionSlot(Set(Position.SF, Position.PF))
    val UTIL = PositionSlot(Set(Position.C, Position.PF, Position.PG, Position.SF, Position.SG))
  }

  object SOC {
    val GK = PositionSlot(Position.GK)
    val D  = PositionSlot(Position.D)
    val M  = PositionSlot(Position.M)
    val F  = PositionSlot(Position.F)
    val FLEX = PositionSlot(Set(Position.D, Position.F, Position.M))
  }
}

case class PositionSlot(accepts: Set[Position.Value], player: Option[Player] = None) {
	def canFillSlot(position: Position.Value) = {
		accepts.contains(position)
	}

  def fillWith(player: Player) = {
    copy(player = Some(player))
  }

  def isEmpty = {
    player.isEmpty
  }
}
