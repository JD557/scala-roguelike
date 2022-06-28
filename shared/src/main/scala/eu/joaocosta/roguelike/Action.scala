package eu.joaocosta.roguelike

import eu.joaocosta.minart.input._

enum Action {
  case QuitGame
  case Movement(dx: Int, dy: Int)
}

object Action {
  def getActions(keyboard: KeyboardInput): List[Action] = {
    keyboard.keysPressed.toList.collect {
      case KeyboardInput.Key.Escape => QuitGame
      case KeyboardInput.Key.Up     => Movement(0, -1)
      case KeyboardInput.Key.Down   => Movement(0, 1)
      case KeyboardInput.Key.Left   => Movement(-1, 0)
      case KeyboardInput.Key.Right  => Movement(1, 0)
    }
  }
}
