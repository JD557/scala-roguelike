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
  private val rng            = scala.util.Random(0) // not really purely functional, but should make games reproducible
  val initialState: AppState = Menu(0)

  case class Menu(cursor: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.MoveCursor(_, dy) =>
        copy(cursor = math.min(math.max(0, cursor + dy), 2))
      case Action.Select =>
        cursor match {
          case 0 => InGame(GameState.initialState(AppState.rng)) // New game
          case 1 => this                                         // TODO Load game
          case 2 => Leaving                                      // Quit Game
          case _ => this
        }
      case _ => this
    }
  }

  case class Pause(gameState: GameState, cursor: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.MoveCursor(_, dy) =>
        copy(cursor = math.min(math.max(0, cursor + dy), 3))
      case Action.ReturnToGame =>
        InGame(gameState)
      case Action.Select =>
        cursor match {
          case 0 => applyAction(Action.ReturnToGame) // Continue
          case 1 => this                             // TODO Save game
          case 2 => Menu(0)                          // Back to Menu
          case 3 => Leaving                          // Quit Game
          case _ => this
        }
      case _ => this
    }
  }

  case class InGame(gameState: GameState) extends AppState {

    def mapState(f: GameState => GameState) =
      copy(gameState = f(gameState))

    def applyAction(action: Action): AppState = action match {
      case Action.PauseGame => Pause(gameState, 0)
      case Action.LookAround(triggerAction, radius) =>
        LookAround(gameState, gameState.player.x, gameState.player.y, triggerAction, radius)
      case Action.ViewHistory   => HistoryView(gameState, 0)
      case Action.ViewInventory => InventoryView(gameState, 0)
      case Action.Wait          => this
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
          .applyAction(Action.Damage(List(target), damage))
      case Action.Damage(target :: Nil, damage) =>
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
      case Action.Damage(targets, damage) =>
        if (targets.isEmpty) applyAction(Action.NothingHappened)
        else applyActions(targets.distinct.map(target => Action.Damage(List(target), damage)))
      case Action.Heal(target :: Nil, amount) =>
        val newTarget       = target.heal(amount)
        val effectiveAmount = newTarget.fighter.hp - target.fighter.hp
        if (effectiveAmount <= 0) applyAction(Action.NothingHappened)
        else
          mapState(_.printLine(Message.Healed(target, effectiveAmount)).updateEntity(target, newTarget))
      case Action.Heal(targets, amount) =>
        if (targets.isEmpty) applyAction(Action.NothingHappened)
        else applyActions(targets.distinct.map(target => Action.Heal(List(target), amount)))
      case Action.ChangeBehavior(target, f) =>
        mapState(_.updateEntity(target, target.updateBehavior(f)))

      case Action.UseItem(source, item) =>
        val updatedEntity = source.removeItem(item)
        if (source.inventory == updatedEntity.inventory) this
        else {
          val action = item.consumeResult(updatedEntity, gameState.entities)
          mapState(
            _.printLine(Message.UsedItem(updatedEntity, item)).updateEntity(source, updatedEntity)
          ).applyAction(action)
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
            val (nextAction, nextBehavior) = npc.ai.next(inGame.gameState.player, inGame.gameState.currentLevel, rng)
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

  case class LookAround(
      currentState: GameState,
      cursorX: Int,
      cursorY: Int,
      triggerAction: List[Entity] => Action,
      radius: Int
  ) extends AppState {
    lazy val selectedEntities: List[Entity] =
      currentState.entities.filter(e =>
        cursorX - radius <= e.x && cursorX + radius >= e.x &&
          cursorY - radius <= e.y && cursorY + radius >= e.y
      )
    def applyAction(action: Action): AppState = action match {
      case Action.ReturnToGame => InGame(currentState)
      case Action.MoveCursor(dx, dy) =>
        val nextCursorX = cursorX + dx
        val nextCursorY = cursorY + dy
        if (
          nextCursorX < 0 || nextCursorY < 0 || nextCursorX >= constants.screenWidth || nextCursorY >= constants.screenHeight
        ) this
        else copy(cursorX = nextCursorX, cursorY = nextCursorY)
      case Action.Select =>
        InGame(currentState).applyAction(triggerAction(selectedEntities))
      case _ => this
    }
  }

  case class HistoryView(currentState: GameState, scroll: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.ReturnToGame => InGame(currentState)
      case Action.MoveCursor(_, dy) =>
        val nextScroll = scroll - dy
        if (nextScroll < 0 || nextScroll >= currentState.messages.size) this
        else copy(scroll = nextScroll)
      case _ => this
    }
  }

  case class InventoryView(currentState: GameState, cursor: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.ReturnToGame => InGame(currentState)
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
      case Action.Select | Action.ReturnToGame => Menu(0)
      case _                                   => this
    }
  }

  case object Leaving extends AppState {
    def applyAction(action: Action): AppState = this
  }
}
