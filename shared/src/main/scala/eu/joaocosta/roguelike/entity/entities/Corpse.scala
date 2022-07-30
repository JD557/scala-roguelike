package eu.joaocosta.roguelike.entity.entities

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.rendering.Window

final case class Corpse(of: Entity) extends Entity {
  val name       = s"${of.name} corpse"
  val sprite     = Window.Sprite('%', Pallete.darkGray)
  val isWalkable = true

  val x                                   = of.x
  val y                                   = of.y
  def setPosition(x: Int, y: Int): Corpse = Corpse(of.setPosition(x, y))
}
