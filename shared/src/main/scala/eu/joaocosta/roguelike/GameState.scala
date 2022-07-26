package eu.joaocosta.roguelike

import eu.joaocosta.roguelike.constants.Message
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.entities._
import eu.joaocosta.roguelike.generator.LevelGenerator
import eu.joaocosta.roguelike.random.Distribution

case class GameState(currentLevel: Level, player: Player, exploredTiles: Set[(Int, Int)], messages: List[Message]) {

  def nextLevel(generator: LevelGenerator): Distribution[GameState] = {
    currentLevel.nextLevel(generator).map { newLevel =>
      GameState(
        newLevel,
        player.copy(x = newLevel.gameMap.upStairs._1, y = newLevel.gameMap.upStairs._2),
        Set.empty,
        messages
      )
    }
  }

  def updateEntity(oldEntity: Entity, newEntity: Entity): GameState = (oldEntity, newEntity) match {
    case (_: Player, p: Player) => copy(player = p)
    case (_: Player, p: Corpse) => copy(player = player.updateFighter(_.copy(hp = 0)))
    case _                      => copy(currentLevel = currentLevel.updateEntity(oldEntity, Some(newEntity)))
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
  def initialState: Distribution[GameState] =
    constants.levelGenerator.generateLevel(0).map { initialLevel =>
      val initialPlayer = Player(initialLevel.playerStart._1, initialLevel.playerStart._2)
      GameState(
        currentLevel = initialLevel,
        player = initialPlayer,
        exploredTiles = Set(),
        messages = List(Message.Welcome)
      )
    }
}
