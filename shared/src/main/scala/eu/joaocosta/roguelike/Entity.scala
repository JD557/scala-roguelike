package eu.joaocosta.roguelike

import eu.joaocosta.minart.graphics._

sealed trait Entity {
  def x: Int
  def y: Int

  def sprite: Window.Sprite
}

object Entity {
  case class Player(x: Int, y: Int) extends Entity {
    def move(dx: Int, dy: Int): Player = copy(x = x + dx, y = y + dy)
    val sprite                         = Window.Sprite('@', Constants.Pallete.white)
  }

  case class Npc(x: Int, y: Int) extends Entity {
    def move(dx: Int, dy: Int): Npc = copy(x = x + dx, y = y + dy)
    val sprite                      = Window.Sprite('@', Constants.Pallete.yellow)
  }
}
