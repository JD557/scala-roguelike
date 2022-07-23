package eu.joaocosta.roguelike.entity.components

import eu.joaocosta.roguelike.Action
import eu.joaocosta.roguelike.entity.Entity

object Consumable {
  trait Component {
    def consumeResult(user: Entity, entities: List[Entity]): Action
  }
}
