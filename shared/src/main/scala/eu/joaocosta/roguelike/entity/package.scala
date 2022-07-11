package eu.joaocosta.roguelike

import eu.joaocosta.roguelike.entity.components._

package object entity {
  type BehaviorEntity   = Entity with Behavior.Component
  type ConsumableEntity = Entity with Consumable.Component
  type FighterEntity    = Entity with Fighter.Component[_]
  type MoveableEntity   = Entity with Moveable.Component[_]
  type InventoryEntity  = Entity with Inventory.Component[_]
}
