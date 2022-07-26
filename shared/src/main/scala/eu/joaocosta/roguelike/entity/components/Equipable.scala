package eu.joaocosta.roguelike.entity.components

import eu.joaocosta.roguelike.Action
import eu.joaocosta.roguelike.entity.Entity

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
