package eu.joaocosta.roguelike.entity

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.Constants
import eu.joaocosta.roguelike.rendering.Window

sealed trait Entity {
  def x: Int
  def y: Int

  def name: String
  def sprite: Window.Sprite
  def fighter: Option[Fighter]
  def ai: Option[Behavior]

  def move(dx: Int, dy: Int): Entity
  def applyDamage(damage: Int): Entity
}

object Entity {
  case class Player(x: Int, y: Int, fighter: Option[Fighter] = Some(Fighter(30, 30, 5, 2))) extends Entity {
    val name   = "Player"
    val sprite = Window.Sprite('@', Constants.Pallete.white)
    val ai     = None

    def move(dx: Int, dy: Int): Player   = copy(x = x + dx, y = y + dy)
    def applyDamage(damage: Int): Player = copy(fighter = fighter.map(_.applyDamage(damage)))
  }

  sealed trait Npc extends Entity {
    def move(dx: Int, dy: Int): Entity.Npc
    def applyDamage(damage: Int): Entity.Npc
  }
  case class Orc(x: Int, y: Int, fighter: Option[Fighter] = Some(Fighter(10, 10, 3, 0))) extends Npc {
    def name   = "Orc"
    val sprite = Window.Sprite('o', Constants.Pallete.darkGreen)
    val ai     = Some(Behavior.JustStare)

    def move(dx: Int, dy: Int): Orc   = copy(x = x + dx, y = y + dy)
    def applyDamage(damage: Int): Orc = copy(fighter = fighter.map(_.applyDamage(damage)))
  }
  case class Troll(x: Int, y: Int, fighter: Option[Fighter] = Some(Fighter(16, 16, 4, 1))) extends Npc {
    def name   = "Troll"
    val sprite = Window.Sprite('T', Constants.Pallete.green)
    val ai     = Some(Behavior.Hostile)

    def move(dx: Int, dy: Int): Troll   = copy(x = x + dx, y = y + dy)
    def applyDamage(damage: Int): Troll = copy(fighter = fighter.map(_.applyDamage(damage)))
  }
}
