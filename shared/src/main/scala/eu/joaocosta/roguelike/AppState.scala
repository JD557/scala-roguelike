package eu.joaocosta.roguelike

import eu.joaocosta.roguelike.AppState._

sealed trait AppState {
  def applyAction(action: Action): AppState
}

case class InGame(player: Player) extends AppState {
  def toWindow: Window = Window(
    Map(
      (player.x, player.y) -> ('@', Constants.Pallete.yellow)
    )
  )

  def applyAction(action: Action): AppState = action match {
    case Action.QuitGame         => Leaving
    case Action.Movement(dx, dy) => copy(player = player.move(dx, dy))
  }
}

case object Leaving extends AppState {
  def applyAction(action: Action): AppState = this
}

object AppState {
  val initialState: AppState =
    InGame(Player(x = Constants.screenWidth / 2, y = Constants.screenHeight / 2))

  case class Player(x: Int, y: Int) {
    def move(dx: Int, dy: Int): Player = copy(x = x + dx, y = y + dy)
  }
}
