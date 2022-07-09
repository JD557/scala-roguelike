package eu.joaocosta.roguelike

import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.entity.Entity

enum Action {
  case QuitGame
  case PlayerMovement(dx: Int, dy: Int)
  case PlayerAttack(npc: Entity.Npc)
  case NpcMovement(npc: Entity.Npc, dx: Int, dy: Int)
  case NpcAttack(npc: Entity.Npc)
  case NpcTurn
  case Wait
  case Stare(source: Entity, destination: Entity)
  case SwitchHistoryViewer
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
