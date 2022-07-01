package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.AppState._

sealed trait AppState {
  def applyAction(action: Action): AppState
}

case class InGame(
    gameMap: GameMap,
    player: Entity.Player,
    npcs: List[Entity.Npc],
    exploredTiles: Set[(Int, Int)]
) extends AppState {

  val visibleTiles: Set[(Int, Int)] =
    gameMap.visibleFrom(player.x, player.y, Constants.playerVision)

  val entities: List[Entity] =
    List(player) ++ npcs

  def toWindow: Window = {
    val tileMap = gameMap.tiles.flatMap { case (pos, tile) =>
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
    Window(tileMap ++ entitiesMap)
  }

  def applyAction(action: Action): AppState = action match {
    case Action.QuitGame => Leaving
    case Action.Movement(dx, dy) =>
      if (gameMap.isWalkable(player.x + dx, player.y + dy))
        copy(player = player.move(dx, dy), exploredTiles = exploredTiles ++ visibleTiles)
      else this
  }
}

case object Leaving extends AppState {
  def applyAction(action: Action): AppState = this
}

object AppState {
  val initialState: AppState = {
    val (initialMap, initialPlayer) = MapGenerator
      .generateMap(width = 80, height = 45, roomMaxSize = 10, roomMinSize = 6, maxRooms = 30, random = new Random(0))
    InGame(
      gameMap = initialMap,
      player = initialPlayer,
      npcs = List(Entity.Npc(x = Constants.screenWidth / 2 - 5, y = Constants.screenHeight / 2)),
      exploredTiles = Set()
    )
  }

}
