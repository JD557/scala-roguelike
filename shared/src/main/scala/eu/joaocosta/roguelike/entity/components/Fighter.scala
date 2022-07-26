package eu.joaocosta.roguelike.entity.components

import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities.Equipment

case class Fighter(
    hp: Int,
    maxHp: Int,
    baseAttack: Int,
    baseDefense: Int,
    attackVerb: String,
    expGiven: Int = 0,
    equipment: Map[Equipable.Slot, Equipment] = Map.empty
) {

  val attackBonus  = equipment.values.map(_.attackBonus).sum
  val defenseBonus = equipment.values.map(_.defenseBonus).sum
  val attack       = baseAttack + attackBonus
  val defense      = baseDefense + defenseBonus

  val isDead: Boolean = hp <= 0
  def computeDamage(that: Fighter): Int =
    math.min(math.max(0, this.attack - that.defense), that.hp)
  def updateHp(dHp: Int): Fighter =
    copy(hp = math.min(math.max(0, hp + dHp), maxHp))

  def applyDamage(damage: Int): Fighter =
    updateHp(-damage)
  def heal(amount: Int): Fighter =
    updateHp(amount)

  def equip(item: Equipment): Fighter =
    copy(equipment = equipment + (item.slot -> item))
  def unequip(slot: Equipable.Slot): Fighter =
    copy(equipment = equipment - slot)
}

object Fighter {
  trait Component[E <: Entity with Fighter.Component[E]] {
    def fighter: Fighter
    def updateFighter(f: Fighter => Fighter): E
    def applyDamage(damage: Int): E      = updateFighter(_.applyDamage(damage))
    def heal(amount: Int): E             = updateFighter(_.heal(amount))
    def equip(item: Equipment): E        = updateFighter(_.equip(item))
    def unequip(slot: Equipable.Slot): E = updateFighter(_.unequip(slot))
  }
}
