package eu.joaocosta.roguelike.entity.components

object Equipable {
  enum Slot {
    case Weapon
    case Armor
  }

  trait Component {
    def slot: Slot
    def attackBonus: Int
    def defenseBonus: Int
  }
}
