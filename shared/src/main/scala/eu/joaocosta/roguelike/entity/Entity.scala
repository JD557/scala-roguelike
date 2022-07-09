package eu.joaocosta.roguelike.entity

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.Constants
import eu.joaocosta.roguelike.rendering.Window

sealed trait Entity {
  def x: Int
  def y: Int

  def name: String
  def sprite: Window.Sprite
  def isWalkable: Boolean
}

object Entity {
  case class Player(x: Int, y: Int, fighter: Fighter = Fighter(30, 30, 5, 2))
      extends Entity
      with Moveable.Component[Player]
      with Fighter.Component[Player] {
    val name = "Player"
    val sprite =
      if (fighter.isDead) Window.Sprite('%', Constants.Pallete.white)
      else Window.Sprite('@', Constants.Pallete.white)
    val isWalkable = false

    def move(dx: Int, dy: Int): Player               = copy(x = x + dx, y = y + dy)
    def updateFighter(f: Fighter => Fighter): Player = copy(fighter = f(fighter))
  }

  sealed trait Npc extends Entity with Moveable.Component[Npc] with Fighter.Component[Npc] with Behavior.Component {
    val isWalkable = false
  }
  case class Orc(x: Int, y: Int, fighter: Fighter = Fighter(10, 10, 3, 0)) extends Npc {
    val name   = "Orc"
    val sprite = Window.Sprite('o', Constants.Pallete.darkGreen)

    def move(dx: Int, dy: Int): Orc               = copy(x = x + dx, y = y + dy)
    def updateFighter(f: Fighter => Fighter): Orc = copy(fighter = f(fighter))
    val ai                                        = Behavior.JustStare
  }
  case class Troll(x: Int, y: Int, fighter: Fighter = Fighter(16, 16, 4, 1)) extends Npc {
    val name   = "Troll"
    val sprite = Window.Sprite('T', Constants.Pallete.green)
    val ai     = Behavior.Hostile

    def move(dx: Int, dy: Int): Troll               = copy(x = x + dx, y = y + dy)
    def updateFighter(f: Fighter => Fighter): Troll = copy(fighter = f(fighter))
  }
  case class Corpse(of: Entity) extends Entity {
    val name       = s"${of.name} corpse"
    val sprite     = Window.Sprite('%', Constants.Pallete.darkGray)
    val isWalkable = true

    val x = of.x
    val y = of.y
  }
}
