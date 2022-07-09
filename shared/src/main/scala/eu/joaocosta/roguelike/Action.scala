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
  case Movement(target: MoveableEntity, dx: Int, dy: Int)
  case Attack(source: FighterEntity, target: FighterEntity)
  case Heal(target: FighterEntity, amount: Int)
  case UseItem(source: Entity, target: Entity, item: ConsumableEntity)
  case NpcTurn
}

object Action {
  def getActions(keyboard: KeyboardInput): List[Action] = {
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Escape => List(QuitGame)
      case KeyboardInput.Key.Up     => List(PlayerMovement(0, -1))
      case KeyboardInput.Key.Down   => List(PlayerMovement(0, 1))
      case KeyboardInput.Key.Left   => List(PlayerMovement(-1, 0))
      case KeyboardInput.Key.Right  => List(PlayerMovement(1, 0))
      case KeyboardInput.Key.Space  => List(NpcTurn)
      case KeyboardInput.Key.V      => List(SwitchHistoryViewer)
      case _                        => Nil
    }
  }
}
