package com.andbutso.jester

object PlayerData {
	val dataRoot = System.getenv("JESTER") + "/data"
	val path = "nba-players-10-20-2015.csv"

  // TODO Deal with this mutable state which exists still only so PlayerRef has a global
  // lookup
  var all: AllPlayers = null
	var allById: Map[String, Player] = null

  def makePath(file: String) = s"$dataRoot/$file"

  def loadRosterFromCSV(
    path: String,
    rosterRequirement: RosterRequirement,
    allPlayers: AllPlayers = all
  ) = {
    Rosters(
      loadLinesFromFile(path, skipHeader = false).map { line =>
        val players = line.map { name => allPlayers(name).head }
        PossiblePlayers(rosterRequirement, players)
      }
    )
  }

  def apply(fileName: String) = {
    val ap = AllPlayers(
      playersFromLines(
        loadLinesFromFile(fileName)
      )
    )

    all = ap
    allById = all.players.map { player => player.id -> player }.toMap
    ap
  }

  def loadLinesFromFile(name: String, skipHeader: Boolean = true) = {
    val lines = io.Source.fromFile(
      makePath(name)
    ).getLines().toArray

    val linesToInclude = if (skipHeader) lines.tail else lines
    linesToInclude.map {
      _.split(",")
    }
  }

  def playersFromLines(lines: Array[Array[String]]) = {
    lines collect {
      case (Array(playerName, salary, projectedPoints, position, id, injuryStatus)) if !playerName.startsWith("#") =>
        Player(playerName, salary.toInt, projectedPoints.toFloat, Position.withName(position), id, injuryStatus)
    } filter { _.hasValue }
  }

	def load = {
		apply(path)
	}
}