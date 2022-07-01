package eu.joaocosta.roguelike

import eu.joaocosta.minart.input._

enum Action {
  case QuitGame
  case Movement(dx: Int, dy: Int)
  case EnemyTurn
}

object Action {
  def getActions(keyboard: KeyboardInput): List[Action] = {
    keyboard.keysPressed.toList.flatMap {
      case KeyboardInput.Key.Escape => List(QuitGame)
      case KeyboardInput.Key.Up     => List(Movement(0, -1), EnemyTurn)
      case KeyboardInput.Key.Down   => List(Movement(0, 1), EnemyTurn)
      case KeyboardInput.Key.Left   => List(Movement(-1, 0), EnemyTurn)
      case KeyboardInput.Key.Right  => List(Movement(1, 0), EnemyTurn)
      case _                        => Nil
    }
  }
}
