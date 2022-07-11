package eu.joaocosta.roguelike

import scala.util.Random

import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.constants.Message
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.entities._

sealed trait AppState {
  def applyAction(action: Action): AppState
  def applyActions(actions: Seq[Action]): AppState = actions.foldLeft(this)((st, a) => st.applyAction(a))
}

object AppState {

  val initialState: AppState = {
    val initialLevel = constants.levelGenerator.generateLevel(new Random(0))
    InGame(
      currentLevel = initialLevel,
      player = initialLevel.playerStart,
      exploredTiles = Set(),
      messages = List(Message.Welcome)
    )
  }

  case class InGame(
      currentLevel: Level,
      player: Player,
      exploredTiles: Set[(Int, Int)],
      messages: List[Message]
  ) extends AppState {

    def updateEntity(oldEntity: Entity, newEntity: Entity): InGame = newEntity match {
      case p: Player => copy(player = p)
      case _         => copy(currentLevel = currentLevel.updateEntity(oldEntity, Some(newEntity)))
    }

    val visibleTiles: Set[(Int, Int)] =
      currentLevel.gameMap.visibleFrom(player.x, player.y, constants.playerVision)

    val entities: List[Entity] =
      List(player) ++ currentLevel.entities

    def printLine(message: Message): InGame =
      copy(messages = (message :: messages))

    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame              => Leaving
      case Action.SwitchHistoryViewer   => HistoryView(this, 0)
      case Action.SwitchInventoryViewer => InventoryView(this, 0)
      case Action.Wait                  => this
      case Action.NothingHappened =>
        printLine(Message.NothingHappened)
      case Action.Stare(source, destination) =>
        printLine(Message.Stare(source, destination))
      case Action.PlayerAction(getActions) =>
        applyActions(getActions(player)).applyAction(Action.NpcTurn)
      case Action.Movement(player: Player, dx, dy) =>
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
      case Action.PickUp =>
        currentLevel.items.find(item => item.x == player.x && item.y == player.y) match {
          case Some(item) =>
            if (player.inventory.isFull) printLine(Message.InventoryFull(player))
            else
              printLine(Message.PickedUp(player, item))
                .updateEntity(player, player.addItem(item))
                .copy(currentLevel = currentLevel.updateEntity(item, None))
                .applyAction(Action.NpcTurn)
          case None =>
            applyAction(Action.NothingHappened)
        }
      case Action.Attack(source, target) =>
        val damage = source.fighter.computeDamage(target.fighter)
        printLine(Message.Attacked(source, target, source.fighter.attackVerb))
          .applyAction(Action.Damage(target, damage))
      case Action.Damage(target, damage) =>
        val newTarget = target.applyDamage(damage)
        val afterMessages =
          if (newTarget.fighter.isDead)
            printLine(Message.Damaged(target, damage))
              .printLine(Message.Died(target))
          else printLine(Message.Damaged(target, damage))
        newTarget match {
          case newPlayer: Player =>
            if (newPlayer.fighter.isDead)
              GameOver(finalState = afterMessages.copy(player = newPlayer))
            else
              afterMessages.updateEntity(player, newPlayer)
          case _ =>
            afterMessages.updateEntity(
              target,
              if (!newTarget.fighter.isDead) newTarget else Corpse(newTarget)
            )
        }
      case Action.Heal(target, amount) =>
        val newTarget       = target.heal(amount)
        val effectiveAmount = newTarget.fighter.hp - target.fighter.hp
        if (effectiveAmount <= 0) applyAction(Action.NothingHappened)
        else
          printLine(Message.Healed(target, effectiveAmount)).updateEntity(target, newTarget)
      case Action.UseItem(source, item) =>
        val updatedEntity = source.removeItem(item)
        if (source.inventory == updatedEntity.inventory) this
        else
          item.pickTarget(source, currentLevel.npcs) match {
            case Some(target) =>
              printLine(Message.UsedItem(source, target, item))
                .updateEntity(source, updatedEntity)
                .applyActions(item.consumeResult(if (source == target) updatedEntity else target))
            case None =>
              applyAction(Action.NothingHappened)
          }
      case Action.DropItem(source, item) =>
        val updatedEntity = source.removeItem(item)
        if (source.inventory != updatedEntity.inventory)
          printLine(Message.DroppedItem(source, item))
            .updateEntity(source, updatedEntity)
            .copy(currentLevel = currentLevel.addEntity(item.setPosition(source.x, source.y)))
        else this

      case Action.NpcTurn =>
        currentLevel.npcs.foldLeft(this: AppState) { case (st, npc) =>
          st.applyAction(npc.ai.nextAction(npc, player, currentLevel))
        }
      case _ => this
    }
  }

  case class HistoryView(currentState: InGame, scroll: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame            => Leaving
      case Action.SwitchHistoryViewer => currentState
      case Action.MoveCursor(dy) =>
        val nextScroll = scroll - dy
        if (nextScroll < 0 || nextScroll >= currentState.messages.size) this
        else copy(scroll = nextScroll)
      case _ => this
    }
  }

  case class InventoryView(currentState: InGame, cursor: Int) extends AppState {
    def applyAction(action: Action): AppState = action match {
      case Action.QuitGame              => Leaving
      case Action.SwitchInventoryViewer => currentState
      case Action.MoveCursor(dy) =>
        val nextCursor = cursor + dy
        if (nextCursor < 0 || nextCursor >= currentState.player.inventory.items.size) this
        else copy(cursor = nextCursor)
      case playerAction: Action.PlayerAction =>
        currentState.applyAction(playerAction)
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
