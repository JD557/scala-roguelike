package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.constants.Message
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.entities._

case class GameState(currentLevel: Level, player: Player, exploredTiles: Set[(Int, Int)], messages: List[Message]) {

  def updateEntity(oldEntity: Entity, newEntity: Entity): GameState = newEntity match {
    case p: Player => copy(player = p)
    case _         => copy(currentLevel = currentLevel.updateEntity(oldEntity, Some(newEntity)))
  }

  def addEntity(entity: Entity): GameState =
    copy(currentLevel = currentLevel.addEntity(entity))

  def removeEntity(entity: Entity): GameState =
    copy(currentLevel = currentLevel.updateEntity(entity, None))

  def mapPlayer(f: Player => Player): GameState =
    copy(player = f(player))

  def exploreTiles(newTiles: Set[(Int, Int)]): GameState =
    copy(exploredTiles = exploredTiles ++ newTiles)

  val visibleTiles: Set[(Int, Int)] =
    currentLevel.gameMap.visibleFrom(player.x, player.y, constants.playerVision)

  val entities: List[Entity] =
    List(player) ++ currentLevel.entities

  def printLine(message: Message): GameState =
    copy(messages = (message :: messages))

}

object GameState {
  def initialState(rng: Random): GameState = {
    val initialLevel = constants.levelGenerator.generateLevel(rng)
    GameState(
      currentLevel = initialLevel,
      player = initialLevel.playerStart,
      exploredTiles = Set(),
      messages = List(Message.Welcome)
    )
  }
}
