package eu.joaocosta.roguelike.entity.entities

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components._
import eu.joaocosta.roguelike.rendering.Window
import eu.joaocosta.roguelike.{Action, constants}

sealed trait Npc extends Entity with Moveable.Component[Npc] with Fighter.Component[Npc] with Behavior.Component {
  val isWalkable = false
}

object Npc {
  case class Orc(x: Int, y: Int, fighter: Fighter = Fighter(10, 10, 3, 0, "punched")) extends Npc {
    val name   = "Orc"
    val sprite = Window.Sprite('o', Pallete.darkGreen)

    def move(dx: Int, dy: Int): Orc               = copy(x = x + dx, y = y + dy)
    def updateFighter(f: Fighter => Fighter): Orc = copy(fighter = f(fighter))
    val ai                                        = Behavior.JustStare(constants.playerVision)
  }
  case class Troll(x: Int, y: Int, fighter: Fighter = Fighter(16, 16, 4, 1, "clubed")) extends Npc {
    val name   = "Troll"
    val sprite = Window.Sprite('T', Pallete.green)
    val ai     = Behavior.Hostile(constants.playerVision)

    def move(dx: Int, dy: Int): Troll               = copy(x = x + dx, y = y + dy)
    def updateFighter(f: Fighter => Fighter): Troll = copy(fighter = f(fighter))
  }
}
