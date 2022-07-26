package eu.joaocosta.roguelike.entity.entities

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components._
import eu.joaocosta.roguelike.rendering.Window
import eu.joaocosta.roguelike.{Action, constants}

final case class Player(
    x: Int,
    y: Int,
    exp: Int = 0,
    fighter: Fighter =
      Fighter(constants.baseHp, constants.baseHp, constants.baseAttack, constants.baseDefense, "kicked"),
    inventory: Inventory = Inventory(Nil, 26)
) extends Entity
    with Moveable.Component[Player]
    with Fighter.Component[Player]
    with Inventory.Component[Player] {
  val level = Player.level(exp)

  val name = "Player"
  val sprite =
    if (fighter.isDead) Window.Sprite('%', Pallete.white)
    else Window.Sprite('@', Pallete.white)
  val isWalkable = false

  def setPosition(x: Int, y: Int): Player                = copy(x = x, y = y)
  def move(dx: Int, dy: Int): Player                     = copy(x = x + dx, y = y + dy)
  def updateFighter(f: Fighter => Fighter): Player       = copy(fighter = f(fighter))
  def updateInventory(f: Inventory => Inventory): Player = copy(inventory = f(inventory))
  def addExp(dExp: Int)                                  = copy(exp = exp + dExp)
}

object Player {
  def level(exp: Int, base: Int = constants.baseExp, factor: Int = constants.levelUpFactor): Int =
    if (exp <= base) 1
    else ((1 + math.sqrt(1 + 8.0 * (exp - base) / factor)) / 2).toInt + 1
  def nextLevel(currentLevel: Int, base: Int = constants.baseExp, factor: Int = constants.levelUpFactor) =
    if (currentLevel <= 1) base
    else base + (factor / 2) * (currentLevel) * (currentLevel + 1)
}
