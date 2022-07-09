package eu.joaocosta.roguelike.entity

object Moveable {
  trait Component[E <: Entity with Moveable.Component[E]] {
    def move(dx: Int, dy: Int): E
  }
}
