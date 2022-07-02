package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.entity.Entity

sealed trait AppState {
  def applyAction(action: Action): AppState
  def applyActions(actions: Seq[Action]): AppState = actions.foldLeft(this)((st, a) => st.applyAction(a))
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
      case Action.Wait     => this
      case Action.PlayerMovement(dx, dy) =>
        val nextX = player.x + dx
        val nextY = player.y + dy
        val nextState =
          if (currentLevel.isWalkable(nextX, nextY))
            copy(player = player.move(dx, dy), exploredTiles = exploredTiles ++ visibleTiles)
          else
            currentLevel.npcs.find(npc => npc.x == nextX && npc.y == nextY) match {
              case Some(npc) => applyAction(Action.PlayerAttack(npc))
              case None      => this
            }
        nextState.applyAction(Action.NpcTurn)
      case Action.PlayerAttack(npc) =>
        val damage = player.fighter.fold(0)(_.computeDamage(npc.fighter))
        val newNpc = npc.applyDamage(damage)
        val message =
          if (newNpc.fighter.exists(_.isDead)) s"You killed the ${npc.name}"
          else s"You kicked the ${npc.name} for $damage damage"
        printLine(message).copy(currentLevel =
          currentLevel.updateNpc(npc, Option.when(!newNpc.fighter.exists(_.isDead))(newNpc))
        )
      case Action.NpcMovement(npc, dx, dy) =>
        val nextX = npc.x + dx
        val nextY = npc.y + dy
        if (nextX == player.x && nextY == player.y)
          applyAction(Action.NpcAttack(npc))
        else if (currentLevel.isWalkable(nextX, nextY))
          copy(currentLevel = currentLevel.updateNpc(npc, Some(npc.move(dx, dy))))
        else
          this
      case Action.NpcAttack(npc) =>
        val damage    = npc.fighter.fold(0)(_.computeDamage(player.fighter))
        val newPlayer = player.applyDamage(damage)
        val message =
          if (newPlayer.fighter.exists(_.isDead)) s"You have been killed by ${npc.name}"
          else s"${npc.name} kicked you for $damage damage"
        printLine(message).copy(player = newPlayer)
      case Action.NpcTurn =>
        currentLevel.npcs.foldLeft(this: AppState) { case (st, npc) =>
          npc.ai.fold(st)(ai => st.applyAction(ai.nextAction(npc, player, currentLevel)))
        }
      case Action.Stare(source, destination) =>
        printLine(s"${source.name} is looking at ${destination.name}")
    }
  }

  case object Leaving extends AppState {
    def applyAction(action: Action): AppState = this
  }
}
