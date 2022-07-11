package eu.joaocosta.roguelike.entity.entities

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.Action
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components._
import eu.joaocosta.roguelike.rendering.Window

final case class Player(
    x: Int,
    y: Int,
    fighter: Fighter = Fighter(30, 30, 5, 2, "kicked"),
    inventory: Inventory = Inventory(Nil, 26)
) extends Entity
    with Moveable.Component[Player]
    with Fighter.Component[Player]
    with Inventory.Component[Player] {
  val name = "Player"
  val sprite =
    if (fighter.isDead) Window.Sprite('%', Pallete.white)
    else Window.Sprite('@', Pallete.white)
  val isWalkable = false

  def move(dx: Int, dy: Int): Player                     = copy(x = x + dx, y = y + dy)
  def updateFighter(f: Fighter => Fighter): Player       = copy(fighter = f(fighter))
  def updateInventory(f: Inventory => Inventory): Player = copy(inventory = f(inventory))
}
