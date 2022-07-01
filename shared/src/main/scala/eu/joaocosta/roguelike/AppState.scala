package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.AppState._

sealed trait AppState {
  def applyAction(action: Action): AppState
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

  def toWindow: Window = {
    val tileMap = currentLevel.gameMap.tiles.flatMap { case (pos, tile) =>
      if (visibleTiles(pos)) Some(pos -> tile.sprite)
      else if (exploredTiles(pos)) Some(pos -> tile.darkSprite)
      else None
    }.toMap
    val entitiesMap = entities
      .filter(e => visibleTiles(e.x, e.y))
      .map { entity =>
        val sprite = tileMap
          .get(entity.x, entity.y)
          .fold(entity.sprite)(tile => entity.sprite.copy(bg = tile.bg))
        (entity.x, entity.y) -> sprite
      }
      .toMap
    val messagesMap = (for {
      (textMessage, y) <- messages.zipWithIndex
      (char, x)        <- textMessage.zipWithIndex
      color =
        if (y == 0) Constants.Pallete.white
        else if (y == 1) Constants.Pallete.gray
        else Constants.Pallete.darkGray
      sprite = Window.Sprite(char, color)
    } yield (x, Constants.screenHeight - 1 - y) -> sprite).toMap
    Window(tileMap ++ entitiesMap ++ messagesMap)
  }

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

object AppState {
  val initialState: AppState = {
    val initialLevel = LevelGenerator
      .generateLevel(
        width = 80,
        height = 45,
        roomMaxSize = 10,
        roomMinSize = 6,
        maxRooms = 30,
        maxMonsters = 2,
        random = new Random(0)
      )
    InGame(
      currentLevel = initialLevel,
      player = initialLevel.playerStart,
      exploredTiles = Set(),
      messages = List("Welcome to the Dungeon!")
    )
  }

}
