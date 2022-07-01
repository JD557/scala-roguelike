package eu.joaocosta.roguelike

import eu.joaocosta.minart.graphics._

sealed trait Entity {
  def x: Int
  def y: Int

  def name: String
  def sprite: Window.Sprite
}

object Entity {
  case class Player(x: Int, y: Int) extends Entity {
    val name                           = "Player"
    val sprite                         = Window.Sprite('@', Constants.Pallete.white)
    def move(dx: Int, dy: Int): Player = copy(x = x + dx, y = y + dy)
  }

  sealed trait Npc extends Entity
  case class Orc(x: Int, y: Int) extends Npc {
    def name                        = "Orc"
    val sprite                      = Window.Sprite('o', Constants.Pallete.darkGreen)
    def move(dx: Int, dy: Int): Npc = copy(x = x + dx, y = y + dy)
  }
  case class Troll(x: Int, y: Int) extends Npc {
    def name                        = "Troll"
    val sprite                      = Window.Sprite('T', Constants.Pallete.green)
    def move(dx: Int, dy: Int): Npc = copy(x = x + dx, y = y + dy)
  }
}
