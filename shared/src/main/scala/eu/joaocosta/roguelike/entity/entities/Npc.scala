package eu.joaocosta.roguelike.entity.entities

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.constants
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components.Equipable.Slot
import eu.joaocosta.roguelike.entity.components._
import eu.joaocosta.roguelike.rendering.Window

final case class Npc(x: Int, y: Int, name: String, sprite: Window.Sprite, fighter: Fighter, ai: Behavior)
    extends Entity
    with Moveable.Component[Npc]
    with Fighter.Component[Npc]
    with Behavior.Component[Npc] {
  val isWalkable                                   = false
  def setPosition(x: Int, y: Int): Npc             = copy(x = x, y = y)
  def move(dx: Int, dy: Int): Npc                  = copy(x = x + dx, y = y + dy)
  def updateFighter(f: Fighter => Fighter): Npc    = copy(fighter = f(fighter))
  def updateBehavior(f: Behavior => Behavior): Npc = copy(ai = f(ai))
}

object Npc {
  val Bat = Npc.Builder(
    name = "Bat",
    sprite = Window.Sprite('v', Pallete.darkBlue),
    fighter = Fighter(5, 5, 2, 0, 10),
    ai = Behavior.Hostile(constants.playerVision)
  )

  val Spider = Npc.Builder(
    name = "Spider",
    sprite = Window.Sprite('m', Pallete.darkBrown),
    fighter = Fighter(5, 5, 2, 0, 10),
    ai = Behavior.Hostile(constants.playerVision)
  )

  val Goblin = Npc.Builder(
    name = "Goblin",
    sprite = Window.Sprite('g', Pallete.lightGreen),
    fighter = Fighter(10, 10, 3, 0, 35, Map(Slot.Weapon -> Equipment.Dagger(0, 0))),
    ai = Behavior.Hostile(constants.playerVision)
  )

  val Orc = Npc.Builder(
    name = "Orc",
    sprite = Window.Sprite('O', Pallete.lightGreen),
    fighter = Fighter(16, 16, 7, 1, 50),
    ai = Behavior.Hostile(constants.playerVision)
  )

  val Troll = Npc.Builder(
    name = "Troll",
    sprite = Window.Sprite('T', Pallete.darkGreen),
    fighter = Fighter(20, 20, 10, 3, 100, Map(Slot.Weapon -> Equipment.Club(0, 0))),
    ai = Behavior.Hostile(constants.playerVision)
  )

  final case class Builder(name: String, sprite: Window.Sprite, fighter: Fighter, ai: Behavior)
      extends Entity.Builder[Npc] {
    def apply(x: Int, y: Int): Npc =
      Npc(x = x, y = y, name = name, sprite = sprite, fighter = fighter, ai = ai)
  }
}
