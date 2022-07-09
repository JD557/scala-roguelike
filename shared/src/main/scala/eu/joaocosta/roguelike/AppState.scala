package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.entity._

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

    def updateEntity(oldEntity: Entity, newEntity: Entity): InGame = newEntity match {
      case p: Entity.Player => copy(player = p)
      case _                => copy(currentLevel = currentLevel.updateEntity(oldEntity, Some(newEntity)))
    }

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
      case Action.NothingHappened =>
        printLine(Constants.Message.NothingHappened)
      case Action.Stare(source, destination) =>
        printLine(Constants.Message.Stare(source, destination))
      case Action.PlayerMovement(dx, dy) =>
        val nextX = player.x + dx
        val nextY = player.y + dy
        if (currentLevel.isWalkable(nextX, nextY) || currentLevel.npcs.exists(npc => npc.x == nextX && npc.y == nextY))
          applyAction(Action.Movement(player, dx, dy)).applyAction(Action.NpcTurn)
        else
          this
      case Action.Movement(player: Entity.Player, dx, dy) =>
        val nextX = player.x + dx
        val nextY = player.y + dy
        if (currentLevel.isWalkable(nextX, nextY))
          copy(player = player.move(dx, dy), exploredTiles = exploredTiles ++ visibleTiles)
        else
          currentLevel.npcs.find(npc => npc.x == nextX && npc.y == nextY) match {
            case Some(npc) => applyAction(Action.Attack(player, npc))
            case None      => this
          }
      case Action.Movement(target, dx, dy) =>
        val nextX     = target.x + dx
        val nextY     = target.y + dy
        val hitPlayer = nextX == player.x && nextY == player.y
        if (currentLevel.isWalkable(nextX, nextY) && !hitPlayer)
          updateEntity(target, target.move(dx, dy))
        else if (target.isInstanceOf[FighterEntity] && hitPlayer)
          applyAction(Action.Attack(target.asInstanceOf[FighterEntity], player))
        else
          this
      case Action.Attack(source, target) =>
        val damage    = source.fighter.computeDamage(target.fighter)
        val newTarget = target.applyDamage(damage)
        val message =
          if (target.fighter.isDead) Constants.Message.Killed(source, target)
          else Constants.Message.Damaged(source, target, damage)
        newTarget match {
          case newPlayer: Entity.Player =>
            if (newPlayer.fighter.isDead)
              GameOver(finalState = printLine(message).copy(player = newPlayer))
            else
              printLine(message).updateEntity(player, newPlayer)
          case _ =>
            printLine(message).updateEntity(
              target,
              if (!newTarget.fighter.isDead) newTarget else Entity.Corpse(newTarget)
            )
        }
      case Action.Heal(target, amount) =>
        val newTarget       = target.heal(amount)
        val effectiveAmount = newTarget.fighter.hp - target.fighter.hp
        if (effectiveAmount <= 0) applyAction(Action.NothingHappened)
        else
          printLine(Constants.Message.Healed(target, effectiveAmount)).updateEntity(target, newTarget)
      case Action.NpcTurn =>
        currentLevel.npcs.foldLeft(this: AppState) { case (st, npc) =>
          st.applyAction(npc.ai.nextAction(npc, player, currentLevel))
        }
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
