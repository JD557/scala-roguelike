package eu.joaocosta.roguelike.entity.components

import eu.joaocosta.roguelike.entity.Entity

object Moveable {
  trait Component[E <: Entity with Moveable.Component[E]] {
    def move(dx: Int, dy: Int): E
  }
}
