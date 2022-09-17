package eu.joaocosta.roguelike

import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components._
import eu.joaocosta.roguelike.entity.entities._

enum Action {
  case PauseGame
  case ReturnToGame
  case LookAround(action: List[Entity] => Action, radius: Int = 0)
  case ViewHelp
  case ViewHistory
  case ViewInventory
  case Wait
  case NothingHappened
  case Stare(source: Entity, destination: Entity)
  case PlayerAction(f: Player => Option[Action])
  case PickUp
  case GoDown
  case Movement(target: MoveableEntity, dx: Int, dy: Int)
  case Attack(source: FighterEntity, target: FighterEntity)
  case Damage(targets: List[FighterEntity], amount: Int)
  case Heal(targets: List[FighterEntity], amount: Int)
  case ChangeBehavior(target: BehaviorEntity, f: Behavior => Behavior)
  case Equip(source: FighterEntity, item: Equipment)
  case Unequip(source: FighterEntity, item: Equipable.Slot)
  case UseItem(source: InventoryEntity, item: ConsumableEntity)
  case DropItem(source: InventoryEntity, item: Entity)
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

  val menuActions: ActionList = Map(
    KeyboardInput.Key.Escape -> ReturnToGame,
    KeyboardInput.Key.Enter  -> Select,
    KeyboardInput.Key.Up     -> MoveCursor(0, -1),
    KeyboardInput.Key.Down   -> MoveCursor(0, 1),
    KeyboardInput.Key.Left   -> MoveCursor(-1, 0),
    KeyboardInput.Key.Right  -> MoveCursor(1, 0)
  )

  val inGameActions: ActionList = playerMovementActions ++ Map(
    KeyboardInput.Key.Escape -> PauseGame,
    KeyboardInput.Key.H      -> ViewHelp,
    KeyboardInput.Key.L      -> LookAround(_ => ReturnToGame),
    KeyboardInput.Key.V      -> ViewHistory,
    KeyboardInput.Key.I      -> ViewInventory,
    KeyboardInput.Key.D      -> GoDown,
    KeyboardInput.Key.G      -> PickUp
  )

  def inventoryViewActions(selectedItem: Option[Either[Equipment, ConsumableEntity]]): ActionList = menuActions ++ Map(
    KeyboardInput.Key.Backspace -> PlayerAction(p => selectedItem.flatMap(_.toOption).map(item => DropItem(p, item))),
    KeyboardInput.Key.Enter -> PlayerAction(p =>
      selectedItem.map {
        case Right(item)     => UseItem(p, item)
        case Left(equipment) => Unequip(p, equipment.slot)
      }
    )
  )

  def getActions(state: AppState, keysPressed: Set[KeyboardInput.Key]): Option[Action] = {
    keysPressed.headOption.flatMap { key => // Assume a single key is pressed. It's safer than to switch key order
      state match {
        case _: AppState.Menu           => menuActions.get(key)
        case _: AppState.Pause          => menuActions.get(key)
        case _: AppState.Help           => menuActions.get(key)
        case _: AppState.InGame         => inGameActions.get(key)
        case _: AppState.LookAround     => menuActions.get(key)
        case _: AppState.HistoryView    => menuActions.get(key)
        case _: AppState.LevelUp        => menuActions.get(key)
        case st: AppState.InventoryView => inventoryViewActions(st.selectedItem).get(key)
        case _: AppState.GameOver       => menuActions.get(key)
        case _                          => None
      }
    }
  }
}
