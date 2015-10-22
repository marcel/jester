package com.andbutso.jester

case class PlayerRef(id: String) extends Valueable {
	def player = PlayerData.allById(id)
	def value  = player.value
	def salary = player.salary
	def name   = player.name

	def position = player.position

	def projectedPoints = player.projectedPoints
	override def toString = s"Ref(${player.toString})"
}