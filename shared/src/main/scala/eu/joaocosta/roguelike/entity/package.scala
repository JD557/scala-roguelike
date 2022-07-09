package eu.joaocosta.roguelike

package object entity {
  type BehaviorEntity   = Entity with Behavior.Component
  type ConsumableEntity = Entity with Consumable.Component
  type FighterEntity    = Entity with Fighter.Component[_]
  type MoveableEntity   = Entity with Moveable.Component[_]
}
