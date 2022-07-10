package eu.joaocosta.roguelike

import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.entity._

enum Action {
  case QuitGame
  case SwitchHistoryViewer
  case Wait
  case NothingHappened
  case Stare(source: Entity, destination: Entity)
  case PlayerMovement(dx: Int, dy: Int)
  case PickUp
  case Movement(target: MoveableEntity, dx: Int, dy: Int)
  case Attack(source: FighterEntity, target: FighterEntity)
  case Heal(target: FighterEntity, amount: Int)
  case UseItem(source: Entity, target: Entity, item: ConsumableEntity)
  case NpcTurn
  case ScrollLog(dy: Int)
}

object Action {
  def getBaseActions(keyboard: KeyboardInput): List[Action] =
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Escape => List(QuitGame)
      case _                        => Nil
    }

  def getInGameActions(keyboard: KeyboardInput): List[Action] =
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Escape => List(QuitGame)
      case KeyboardInput.Key.Up     => List(PlayerMovement(0, -1))
      case KeyboardInput.Key.Down   => List(PlayerMovement(0, 1))
      case KeyboardInput.Key.Left   => List(PlayerMovement(-1, 0))
      case KeyboardInput.Key.Right  => List(PlayerMovement(1, 0))
      case KeyboardInput.Key.Space  => List(NpcTurn)
      case KeyboardInput.Key.V      => List(SwitchHistoryViewer)
      case KeyboardInput.Key.G      => List(PickUp)
      case _                        => Nil
    }

  def getHistoryViewActions(keyboard: KeyboardInput): List[Action] =
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Up   => List(ScrollLog(1))
      case KeyboardInput.Key.Down => List(ScrollLog(-1))
      case KeyboardInput.Key.V    => List(SwitchHistoryViewer)
      case _                      => Nil
    }

  def getActions(state: AppState, keyboard: KeyboardInput): List[Action] = getBaseActions(keyboard) ++ (state match {
    case _: AppState.InGame      => getInGameActions(keyboard)
    case _: AppState.HistoryView => getHistoryViewActions(keyboard)
    case _                       => Nil
  })
}
