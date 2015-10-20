package com.andbutso.jester

object PlayerData {
	val path = "/Users/marcel/Desktop/draftkings-players-10-19-2015.csv"
	// val path = "/Users/marcel/Desktop/ffn-players-10-13-2015.csv"
	val lines = io.Source.fromFile(path).getLines.toArray.tail
	val data = lines map { _.split(",") }
	lazy val all = load
	lazy val allById = all.players.map { player => player.id -> player }.toMap

	def load = {
		AllPlayers(
			data collect {
				case (Array(playerName, salary, projectedPoints, position, id)) if !playerName.startsWith("#") =>
					Player(playerName, salary.toInt, projectedPoints.toFloat, Position.withName(position), id)
			} filter { _.hasValue }
		)
	}
}
