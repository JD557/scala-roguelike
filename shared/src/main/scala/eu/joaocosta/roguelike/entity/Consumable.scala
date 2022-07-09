package eu.joaocosta.roguelike.entity

import eu.joaocosta.roguelike.Action

object Consumable {
  trait Component {
    def consumeResult(target: Entity): List[Action]
  }
}
