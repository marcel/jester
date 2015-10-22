package com.andbutso.jester

object PositionSlot {
	def apply(position: Position.Value): PositionSlot = {
		PositionSlot(Set(position))
	}

	object NFL {
		val QB   = Position.QB.slot
		val WR   = Position.WR.slot
		val RB   = Position.RB.slot
		val TE   = Position.TE.slot
		val FLEX = PositionSlot(Set(Position.WR, Position.RB, Position.TE))
		val DST  = Position.DST.slot
	}

	object CFB {
		val QB = Position.QB.slot
		val RB = Position.RB.slot
		val WR = Position.WR.slot
		val FLEX = PositionSlot(Set(Position.RB, Position.WR))
	}

  object NBA {
    val PG = Position.PG.slot
    val SG = Position.SG.slot
    val SF = Position.SF.slot
    val PF = Position.PF.slot
    val C  = Position.C.slot
    val G  = PositionSlot(Set(Position.PG, Position.SG))
    val F  = PositionSlot(Set(Position.SF, Position.PF))
    val UTIL = PositionSlot(Set(Position.C, Position.PF, Position.PG, Position.SF, Position.SG))
  }

  object CBB {
    val G = Position.G.slot
    val F = Position.F.slot
    val UTIL = PositionSlot(Set(Position.F, Position.G))
  }

  object SOC {
    val GK = Position.GK.slot
    val D  = Position.D.slot
    val M  = Position.M.slot
    val F  = Position.F.slot
    val FLEX = PositionSlot(Set(Position.D, Position.F, Position.M))
  }

  object NHL {
    val W = PositionSlot(Set(Position.RW, Position.LW))
    val C = Position.C.slot
    val D = Position.D.slot
    val G = Position.G.slot
    val UTIL = PositionSlot(Set(Position.LW, Position.RW, Position.C, Position.D))
  }

  object MLB {
    val P    = Position.P.slot
    val C    = Position.C.slot
    val FBDH = Position.FBDH.slot
    val SB   = Position.SB.slot
    val TB   = Position.TB.slot
    val SS   = Position.SS.slot

  }
}

case class PositionSlot(accepts: Set[Position.Value], player: Option[Player] = None) {
  def interchangeableWith(otherSlot: PositionSlot) = {
    accepts.intersect(otherSlot.accepts).nonEmpty
  }

  def isFlex = accepts.size > 1

	def canBeFilledBy(position: Position.Value) = {
		accepts.contains(position)
	}

  def canBeFilledBy(player: Player) = {
    accepts.contains(player.position)
  }

  def isFilled = {
    player.nonEmpty
  }

  def isEmpty = {
    player.isEmpty
  }

  def fillWith(player: Player) = {
    if (!isFilled && canBeFilledBy(player)) {
      copy(player = Some(player))
    } else {
      this
    }
  }
}
