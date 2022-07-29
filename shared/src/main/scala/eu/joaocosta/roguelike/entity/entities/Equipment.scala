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
  val Club = Weapon.Builder(
    name = "Club",
    sprite = Window.Sprite('|', Pallete.brown),
    attackBonus = 1,
    attackVerb = "clubbed"
  )

  val Dagger = Weapon.Builder(
    name = "Dagger",
    sprite = Window.Sprite('\\', Pallete.yellow),
    attackBonus = 2,
    attackVerb = "stabbed"
  )

  val Sword = Weapon.Builder(
    name = "Sword",
    sprite = Window.Sprite('\\', Pallete.orange),
    attackBonus = 4,
    attackVerb = "sliced"
  )

  val LeatherArmor = Armor.Builder(
    name = "Leather Armor",
    sprite = Window.Sprite(']', Pallete.orange),
    defenseBonus = 1
  )

  val ChainMail = Armor.Builder(
    name = "Chain Mail",
    sprite = Window.Sprite(']', Pallete.orange),
    defenseBonus = 3
  )

  final case class Weapon(x: Int, y: Int, name: String, sprite: Window.Sprite, attackBonus: Int, attackVerb: String)
      extends Equipment {
    val slot                                = Slot.Weapon
    val defenseBonus                        = 0
    def setPosition(x: Int, y: Int): Weapon = copy(x = x, y = y)
  }
  object Weapon {
    final case class Builder(name: String, sprite: Window.Sprite, attackBonus: Int, attackVerb: String)
        extends Entity.Builder[Weapon] {
      def apply(x: Int, y: Int): Weapon = {
        Weapon(
          x = x,
          y = y,
          name = name,
          sprite = sprite,
          attackBonus = attackBonus,
          attackVerb = attackVerb
        )
      }
    }
  }
  final case class Armor(x: Int, y: Int, name: String, sprite: Window.Sprite, defenseBonus: Int) extends Equipment {
    val slot                               = Slot.Armor
    val attackBonus                        = 0
    def setPosition(x: Int, y: Int): Armor = copy(x = x, y = y)
  }
  object Armor {
    final case class Builder(name: String, sprite: Window.Sprite, defenseBonus: Int) extends Entity.Builder[Armor] {
      def apply(x: Int, y: Int): Armor = {
        Armor(
          x = x,
          y = y,
          name = name,
          sprite = sprite,
          defenseBonus = defenseBonus
        )
      }
    }
  }

}
