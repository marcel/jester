package com.andbutso.jester

object PlayerData {
	val dataRoot = System.getenv("JESTER") + "/data"
	val path = "nba-players-10-20-2015.csv"
	lazy val all = load
	lazy val allById = all.players.map { player => player.id -> player }.toMap

  def makePath(file: String) = s"$dataRoot/$file"

  def apply(fileName: String) = {
    AllPlayers(
      playersFromLines(
        loadLinesFromFile(fileName)
      )
    )
  }

  def loadLinesFromFile(name: String) = {
    io.Source.fromFile(
      makePath(name)
    ).getLines.toArray.tail.map {
      _.split(",")
    }
  }

  def playersFromLines(lines: Array[Array[String]]) = {
    lines collect {
      case (Array(playerName, salary, projectedPoints, position, id)) if !playerName.startsWith("#") =>
        Player(playerName, salary.toInt, projectedPoints.toFloat, Position.withName(position), id)
    } filter { _.hasValue }
  }

	def load = {
		apply(path)
	}
}