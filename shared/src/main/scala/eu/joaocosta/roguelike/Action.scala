package eu.joaocosta.roguelike

import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.entity._

enum Action {
  case QuitGame
  case SwitchHistoryViewer
  case SwitchInventoryViewer
  case Wait
  case NothingHappened
  case Stare(source: Entity, destination: Entity)
  case PlayerAction(f: Entity.Player => List[Action])
  case PickUp
  case Movement(target: MoveableEntity, dx: Int, dy: Int)
  case Attack(source: FighterEntity, target: FighterEntity)
  case Damage(target: FighterEntity, amount: Int)
  case Heal(target: FighterEntity, amount: Int)
  case UseItem(source: InventoryEntity, item: Entity.Item)
  case DropItem(source: InventoryEntity, item: Entity.Item)
  case NpcTurn
  case MoveCursor(dy: Int)
}

object Action {
  def getBaseActions(keyboard: KeyboardInput): List[Action] =
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Escape => List(QuitGame)
      case _                        => Nil
    }

  def getInGameActions(keyboard: KeyboardInput): List[Action] =
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Up    => List(PlayerAction(p => List(Movement(p, 0, -1))))
      case KeyboardInput.Key.Down  => List(PlayerAction(p => List(Movement(p, 0, 1))))
      case KeyboardInput.Key.Left  => List(PlayerAction(p => List(Movement(p, -1, 0))))
      case KeyboardInput.Key.Right => List(PlayerAction(p => List(Movement(p, 1, 0))))
      case KeyboardInput.Key.Space => List(NpcTurn)
      case KeyboardInput.Key.V     => List(SwitchHistoryViewer)
      case KeyboardInput.Key.I     => List(SwitchInventoryViewer)
      case KeyboardInput.Key.G     => List(PickUp)
      case _                       => Nil
    }

  def getHistoryViewActions(keyboard: KeyboardInput): List[Action] =
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Up   => List(MoveCursor(-1))
      case KeyboardInput.Key.Down => List(MoveCursor(1))
      case KeyboardInput.Key.V    => List(SwitchHistoryViewer)
      case _                      => Nil
    }

  def getInventoryViewActions(cursor: Int, keyboard: KeyboardInput): List[Action] =
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Up   => List(MoveCursor(-1))
      case KeyboardInput.Key.Down => List(MoveCursor(1))
      case KeyboardInput.Key.I    => List(SwitchInventoryViewer)
      case KeyboardInput.Key.D =>
        List(PlayerAction(p => p.inventory.items.drop(cursor).headOption.map(item => DropItem(p, item)).toList))
      case KeyboardInput.Key.U =>
        List(PlayerAction(p => p.inventory.items.drop(cursor).headOption.map(item => UseItem(p, item)).toList))
      case _ => Nil
    }

  def getActions(state: AppState, keyboard: KeyboardInput): List[Action] = getBaseActions(keyboard) ++ (state match {
    case _: AppState.InGame         => getInGameActions(keyboard)
    case _: AppState.HistoryView    => getHistoryViewActions(keyboard)
    case st: AppState.InventoryView => getInventoryViewActions(st.cursor, keyboard)
    case _                          => Nil
  })
}
