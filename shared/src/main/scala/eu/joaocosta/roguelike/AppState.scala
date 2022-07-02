package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.AppState._

sealed trait AppState {
  def applyAction(action: Action): AppState
}

object AppState {

  val initialState: AppState = {
    val initialLevel = Constants.levelGenerator.generateLevel(new Random(0))
    InGame(
      currentLevel = initialLevel,
      player = initialLevel.playerStart,
      exploredTiles = Set(),
      messages = List("Welcome to the Dungeon!")
    )
  }

  case class InGame(
      currentLevel: Level,
      player: Entity.Player,
      exploredTiles: Set[(Int, Int)],
      messages: List[String]
  ) extends AppState {

    val visibleTiles: Set[(Int, Int)] =
      currentLevel.gameMap.visibleFrom(player.x, player.y, Constants.playerVision)

    val entities: List[Entity] =
      List(player) ++ currentLevel.npcs

    def printLine(message: String) = copy(messages = (message :: messages).take(Constants.maxMessages))

    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame => Leaving
      case Action.Movement(dx, dy) =>
        val nextX = player.x + dx
        val nextY = player.y + dy
        if (currentLevel.isWalkable(nextX, nextY))
          copy(player = player.move(dx, dy), exploredTiles = exploredTiles ++ visibleTiles)
        else
          currentLevel.npcs.find(npc => npc.x == nextX && npc.y == nextY) match {
            case Some(npc) => printLine(s"You kick the ${npc.name}")
            case None      => this
          }
      case Action.EnemyTurn =>
        val visibleEnemies = currentLevel.npcs.filter(npc => visibleTiles((npc.x, npc.y)))
        visibleEnemies.foldLeft(this) { case (st, npc) => st.printLine(s"${npc.name} is looking at ${player.name}") }
    }
  }

  case object Leaving extends AppState {
    def applyAction(action: Action): AppState = this
  }
}
