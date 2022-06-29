package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.AppState._

sealed trait AppState {
  def applyAction(action: Action): AppState
}

case class InGame(gameMap: GameMap, player: Entity.Player, npcs: List[Entity.Npc]) extends AppState {
  val entities: List[Entity] =
    List(player) ++ npcs

  def toWindow: Window = {
    val tileMap = gameMap.tiles.view.mapValues(_.sprite).toMap
    val entitiesMap = entities.map { entity =>
      val sprite = tileMap
        .get(entity.x, entity.y)
        .fold(entity.sprite)(tile => entity.sprite.copy(bg = tile.bg))
      (entity.x, entity.y) -> sprite
    }.toMap
    Window(tileMap ++ entitiesMap)
  }

  def applyAction(action: Action): AppState = action match {
    case Action.QuitGame => Leaving
    case Action.Movement(dx, dy) =>
      if (gameMap.isWalkable(player.x + dx, player.y + dy)) copy(player = player.move(dx, dy))
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
      npcs = List(Entity.Npc(x = Constants.screenWidth / 2 - 5, y = Constants.screenHeight / 2))
    )
  }

}
