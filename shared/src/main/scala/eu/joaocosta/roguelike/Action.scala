package eu.joaocosta.roguelike

import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components.Behavior
import eu.joaocosta.roguelike.entity.entities._

enum Action {
  case QuitGame
  case ReturnToGame
  case LookAround(action: List[Entity] => Action, radius: Int = 0)
  case ViewHistory
  case ViewInventory
  case Wait
  case NothingHappened
  case Stare(source: Entity, destination: Entity)
  case PlayerAction(f: Player => Option[Action])
  case PickUp
  case Movement(target: MoveableEntity, dx: Int, dy: Int)
  case Attack(source: FighterEntity, target: FighterEntity)
  case Damage(targets: List[FighterEntity], amount: Int)
  case Heal(targets: List[FighterEntity], amount: Int)
  case ChangeBehavior(target: BehaviorEntity, f: Behavior => Behavior)
  case UseItem(source: InventoryEntity, item: Item)
  case DropItem(source: InventoryEntity, item: Item)
  case NpcTurn
  case MoveCursor(dx: Int, dy: Int)
  case Select
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
    KeyboardInput.Key.L -> LookAround(_ => ReturnToGame),
    KeyboardInput.Key.V -> ViewHistory,
    KeyboardInput.Key.I -> ViewInventory,
    KeyboardInput.Key.G -> PickUp
  )

  val lookAroundActions: ActionList = cursorMovementActions ++ Map(
    KeyboardInput.Key.L     -> ReturnToGame,
    KeyboardInput.Key.Enter -> Select
  )

  val historyViewActions: ActionList = cursorMovementActions ++ Map(
    KeyboardInput.Key.V -> ReturnToGame
  )

  def inventoryViewActions(cursor: Int): ActionList = cursorMovementActions ++ Map(
    KeyboardInput.Key.I -> ReturnToGame,
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
