package eu.joaocosta.roguelike.entity.entities

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.Action
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components.Equipable.Slot
import eu.joaocosta.roguelike.entity.components._
import eu.joaocosta.roguelike.rendering.Window

sealed trait Equipment extends Entity with Equipable.Component with Consumable.Component {
  val isWalkable = true

  def setPosition(x: Int, y: Int): Equipment

  def consumeResult(user: Entity, entities: List[Entity]): Action =
    user match {
      case fighter: FighterEntity => Action.Equip(fighter, this)
      case _                      => Action.NothingHappened
    }
}

object Equipment {
  case class Dagger(x: Int, y: Int) extends Equipment {
    val name   = "Dagger"
    val sprite = Window.Sprite('/', Pallete.yellow)
    def setPosition(x: Int, y: Int): Dagger =
      copy(x = x, y = y)

    val slot: Slot        = Slot.Weapon
    val attackBonus: Int  = 2
    val defenseBonus: Int = 0
  }

  case class Sword(x: Int, y: Int) extends Equipment {
    val name   = "Sword"
    val sprite = Window.Sprite('/', Pallete.orange)
    def setPosition(x: Int, y: Int): Sword =
      copy(x = x, y = y)

    val slot: Slot        = Slot.Weapon
    val attackBonus: Int  = 4
    val defenseBonus: Int = 0
  }

  case class LeatherArmor(x: Int, y: Int) extends Equipment {
    val name   = "Leather Armor"
    val sprite = Window.Sprite('[', Pallete.yellow)
    def setPosition(x: Int, y: Int): LeatherArmor =
      copy(x = x, y = y)

    val slot: Slot        = Slot.Armor
    val attackBonus: Int  = 0
    val defenseBonus: Int = 1
  }

  case class ChainMail(x: Int, y: Int) extends Equipment {
    val name   = "Chain Mail"
    val sprite = Window.Sprite('[', Pallete.orange)
    def setPosition(x: Int, y: Int): ChainMail =
      copy(x = x, y = y)

    val slot: Slot        = Slot.Armor
    val attackBonus: Int  = 0
    val defenseBonus: Int = 3
  }
}
