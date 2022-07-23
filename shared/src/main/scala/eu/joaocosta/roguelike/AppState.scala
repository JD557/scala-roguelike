package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.constants._
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.entities._

sealed trait AppState {
  def applyAction(action: Action): AppState
  def applyActions(actions: IterableOnce[Action]): AppState = actions.foldLeft(this)((st, a) => st.applyAction(a))
}

object AppState {

  val initialState: AppState = InGame(GameState.initialState)

  case class InGame(gameState: GameState) extends AppState {

    def mapState(f: GameState => GameState) =
      copy(gameState = f(gameState))

    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame              => Leaving
      case Action.SwitchLookAround      => LookAround(gameState, gameState.player.x, gameState.player.y)
      case Action.SwitchHistoryViewer   => HistoryView(gameState, 0)
      case Action.SwitchInventoryViewer => InventoryView(gameState, 0)
      case Action.Wait                  => this
      case Action.NothingHappened =>
        mapState(_.printLine(Message.NothingHappened))
      case Action.Stare(source, destination) =>
        mapState(_.printLine(Message.Stare(source, destination)))
      case Action.PlayerAction(getActions) =>
        applyActions(getActions(gameState.player)).applyAction(Action.NpcTurn)
      case Action.Movement(entity, dx, dy) =>
        val nextX          = entity.x + dx
        val nextY          = entity.y + dy
        lazy val hitPlayer = nextX == gameState.player.x && nextY == gameState.player.y
        lazy val hitNpc =
          gameState.currentLevel.npcs.find(npc => npc.x == nextX && npc.y == nextY)
        lazy val isWalkable = gameState.currentLevel.isWalkable(nextX, nextY) && !hitPlayer
        entity match {
          case _: Player =>
            if (isWalkable)
              mapState(_.mapPlayer(_.move(dx, dy)).exploreTiles(gameState.visibleTiles))
            else
              hitNpc match {
                case Some(npc) => applyAction(Action.Attack(gameState.player, npc))
                case None      => this
              }
          case e: FighterEntity =>
            if (isWalkable)
              mapState(_.updateEntity(entity, entity.move(dx, dy)))
            else if (hitPlayer)
              applyAction(Action.Attack(e, gameState.player))
            else this
          case _ =>
            if (isWalkable)
              mapState(_.updateEntity(entity, entity.move(dx, dy)))
            else this
        }
      case Action.PickUp =>
        gameState.currentLevel.items.find(item => item.x == gameState.player.x && item.y == gameState.player.y) match {
          case Some(item) =>
            if (gameState.player.inventory.isFull)
              mapState(_.printLine(Message.InventoryFull(gameState.player)))
            else
              mapState(
                _.printLine(Message.PickedUp(gameState.player, item))
                  .updateEntity(gameState.player, gameState.player.addItem(item))
                  .removeEntity(item)
              ).applyAction(Action.NpcTurn)
          case None =>
            applyAction(Action.NothingHappened)
        }
      case Action.Attack(source, target) =>
        val damage = source.fighter.computeDamage(target.fighter)
        mapState(_.printLine(Message.Attacked(source, target, source.fighter.attackVerb)))
          .applyAction(Action.Damage(target, damage))
      case Action.Damage(target, damage) =>
        val newTarget = target.applyDamage(damage)
        if (newTarget.fighter.isDead) {
          val nextState = mapState(
            _.printLine(Message.Damaged(target, damage))
              .printLine(Message.Died(target))
              .updateEntity(
                target,
                if (target == gameState.player) newTarget
                else Corpse(newTarget)
              )
          )
          if (target == gameState.player) GameOver(nextState.gameState) else nextState
        } else
          mapState(
            _.printLine(Message.Damaged(target, damage))
              .updateEntity(target, newTarget)
          )
      case Action.Heal(target, amount) =>
        val newTarget       = target.heal(amount)
        val effectiveAmount = newTarget.fighter.hp - target.fighter.hp
        if (effectiveAmount <= 0) applyAction(Action.NothingHappened)
        else
          mapState(_.printLine(Message.Healed(target, effectiveAmount)).updateEntity(target, newTarget))
      case Action.UseItem(source, item) =>
        val updatedEntity = source.removeItem(item)
        if (source.inventory == updatedEntity.inventory) this
        else
          item.pickTarget(source, gameState.currentLevel.npcs) match {
            case Some(target) =>
              mapState(
                _.printLine(Message.UsedItem(source, target, item))
                  .updateEntity(source, updatedEntity)
              ).applyActions(item.consumeResult(if (source == target) updatedEntity else target))
            case None =>
              applyAction(Action.NothingHappened)
          }
      case Action.DropItem(source, item) =>
        val updatedEntity = source.removeItem(item)
        if (source.inventory != updatedEntity.inventory)
          mapState(
            _.printLine(Message.DroppedItem(source, item))
              .updateEntity(source, updatedEntity)
              .addEntity(item.setPosition(source.x, source.y))
          )
        else this

      case Action.NpcTurn =>
        gameState.currentLevel.npcs.foldLeft(this: AppState) {
          case (inGame: InGame, npc) =>
            val (nextAction, nextBehavior) = npc.ai.next(inGame.gameState.player, inGame.gameState.currentLevel)
            val newNpc =
              if (nextBehavior != npc.ai) npc.updateBehavior(_ => nextBehavior)
              else npc
            inGame
              .mapState(_.updateEntity(npc, newNpc))
              .applyAction(nextAction(newNpc))
          case (st, _) =>
            st
        }
      case _ => this
    }
  }

  case class LookAround(currentState: GameState, cursorX: Int, cursorY: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame         => Leaving
      case Action.SwitchLookAround => InGame(currentState)
      case Action.MoveCursor(dx, dy) =>
        val nextCursorX = cursorX + dx
        val nextCursorY = cursorY + dy
        if (
          nextCursorX < 0 || nextCursorY < 0 || nextCursorX >= constants.screenWidth || nextCursorY >= constants.screenHeight
        ) this
        else copy(cursorX = nextCursorX, cursorY = nextCursorY)
      case _ => this
    }
  }

  case class HistoryView(currentState: GameState, scroll: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame            => Leaving
      case Action.SwitchHistoryViewer => InGame(currentState)
      case Action.MoveCursor(_, dy) =>
        val nextScroll = scroll - dy
        if (nextScroll < 0 || nextScroll >= currentState.messages.size) this
        else copy(scroll = nextScroll)
      case _ => this
    }
  }

  case class InventoryView(currentState: GameState, cursor: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame              => Leaving
      case Action.SwitchInventoryViewer => InGame(currentState)
      case Action.MoveCursor(_, dy) =>
        val nextCursor = cursor + dy
        if (nextCursor < 0 || nextCursor >= currentState.player.inventory.items.size) this
        else copy(cursor = nextCursor)
      case playerAction: Action.PlayerAction =>
        InGame(currentState).applyAction(playerAction)
      case _ => this
    }
  }

  case class GameOver(finalState: GameState) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame => Leaving
      case _               => this
    }
  }

  case object Leaving extends AppState {
    def applyAction(action: Action): AppState = this
  }
}
