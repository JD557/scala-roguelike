package eu.joaocosta.roguelike

import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.entities._

enum Action {
  case QuitGame
  case SwitchLookAround
  case SwitchHistoryViewer
  case SwitchInventoryViewer
  case Wait
  case NothingHappened
  case Stare(source: Entity, destination: Entity)
  case PlayerAction(f: Player => Option[Action])
  case PickUp
  case Movement(target: MoveableEntity, dx: Int, dy: Int)
  case Attack(source: FighterEntity, target: FighterEntity)
  case Damage(target: FighterEntity, amount: Int)
  case Heal(target: FighterEntity, amount: Int)
  case UseItem(source: InventoryEntity, item: Item)
  case DropItem(source: InventoryEntity, item: Item)
  case NpcTurn
  case MoveCursor(dx: Int, dy: Int)
}

object Action {
  type ActionList = Map[KeyboardInput.Key, Action]

  val playerMovementActions: ActionList = Map(
    KeyboardInput.Key.Up    -> PlayerAction(p => Some(Movement(p, 0, -1))),
    KeyboardInput.Key.Down  -> PlayerAction(p => Some(Movement(p, 0, 1))),
    KeyboardInput.Key.Left  -> PlayerAction(p => Some(Movement(p, -1, 0))),
    KeyboardInput.Key.Right -> PlayerAction(p => Some(Movement(p, 1, 0))),
    KeyboardInput.Key.Space -> NpcTurn
  )

  val cursorMovementActions: ActionList = Map(
    KeyboardInput.Key.Up    -> MoveCursor(0, -1),
    KeyboardInput.Key.Down  -> MoveCursor(0, 1),
    KeyboardInput.Key.Left  -> MoveCursor(-1, 0),
    KeyboardInput.Key.Right -> MoveCursor(1, 0)
  )

  val inGameActions: ActionList = playerMovementActions ++ Map(
    KeyboardInput.Key.L -> SwitchLookAround,
    KeyboardInput.Key.V -> SwitchHistoryViewer,
    KeyboardInput.Key.I -> SwitchInventoryViewer,
    KeyboardInput.Key.G -> PickUp
  )

  val lookAroundActions: ActionList = cursorMovementActions ++ Map(
    KeyboardInput.Key.L -> SwitchLookAround
  )

  val historyViewActions: ActionList = cursorMovementActions ++ Map(
    KeyboardInput.Key.V -> SwitchHistoryViewer
  )

  def inventoryViewActions(cursor: Int): ActionList = cursorMovementActions ++ Map(
    KeyboardInput.Key.I -> SwitchInventoryViewer,
    KeyboardInput.Key.D -> PlayerAction(p => p.inventory.items.drop(cursor).headOption.map(item => DropItem(p, item))),
    KeyboardInput.Key.U -> PlayerAction(p => p.inventory.items.drop(cursor).headOption.map(item => UseItem(p, item)))
  )

  def getActions(state: AppState, keyboard: KeyboardInput): Option[Action] = {
    keyboard.keysPressed.headOption.flatMap {
      key => // Assume a single key is pressed. It's safer than to switch key orders
        state match {
          case _: AppState.InGame         => inGameActions.get(key)
          case _: AppState.LookAround     => lookAroundActions.get(key)
          case _: AppState.HistoryView    => historyViewActions.get(key)
          case st: AppState.InventoryView => inventoryViewActions(st.cursor).get(key)
          case _                          => None
        }
    }
  }
}
