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
      messages = List(Constants.Message.Welcome)
    )
  }

  case class InGame(
      currentLevel: Level,
      player: Entity.Player,
      exploredTiles: Set[(Int, Int)],
      messages: List[Constants.Message]
  ) extends AppState {

    val visibleTiles: Set[(Int, Int)] =
      currentLevel.gameMap.visibleFrom(player.x, player.y, Constants.playerVision)

    val entities: List[Entity] =
      List(player) ++ currentLevel.entities

    def printLine(message: Constants.Message): InGame =
      copy(messages = (message :: messages))

    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame            => Leaving
      case Action.SwitchHistoryViewer => HistoryView(this, 0)
      case Action.Wait                => this
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
        val damage = player.fighter.computeDamage(npc.fighter)
        val newNpc = npc.applyDamage(damage)
        val message =
          if (newNpc.fighter.isDead) Constants.Message.KilledNpc(npc.name)
          else Constants.Message.DamagedNpc(npc.name, damage)
        printLine(message).copy(currentLevel =
          currentLevel.updateEntity(npc, Some(if (!newNpc.fighter.isDead) newNpc else Entity.Corpse(newNpc)))
        )
      case Action.NpcMovement(npc, dx, dy) =>
        val nextX = npc.x + dx
        val nextY = npc.y + dy
        if (nextX == player.x && nextY == player.y)
          applyAction(Action.NpcAttack(npc))
        else if (currentLevel.isWalkable(nextX, nextY))
          copy(currentLevel = currentLevel.updateEntity(npc, Some(npc.move(dx, dy))))
        else
          this
      case Action.NpcAttack(npc) =>
        val damage    = npc.fighter.computeDamage(player.fighter)
        val newPlayer = player.applyDamage(damage)
        if (newPlayer.fighter.isDead)
          GameOver(finalState = printLine(Constants.Message.KilledBy(npc.name)).copy(player = newPlayer))
        else
          printLine(Constants.Message.DamagedBy(npc.name, damage)).copy(player = newPlayer)
      case Action.NpcTurn =>
        currentLevel.npcs.foldLeft(this: AppState) { case (st, npc) =>
          st.applyAction(npc.ai.nextAction(npc, player, currentLevel))
        }
      case Action.Stare(source, destination) =>
        printLine(Constants.Message.Stare(source.name, destination.name))
    }
  }

  case class HistoryView(currentState: InGame, scroll: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame            => Leaving
      case Action.SwitchHistoryViewer => currentState
      case Action.PlayerMovement(_, dy) =>
        val nextScroll = scroll - dy
        if (nextScroll < 0 || nextScroll >= currentState.messages.size) this
        else copy(scroll = nextScroll)
      case _ => this
    }
  }

  case class GameOver(finalState: InGame) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame => Leaving
      case _               => this
    }
  }

  case object Leaving extends AppState {
    def applyAction(action: Action): AppState = this
  }
}
