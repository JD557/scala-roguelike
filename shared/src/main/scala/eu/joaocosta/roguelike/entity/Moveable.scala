package eu.joaocosta.roguelike.entity

object Moveable {
  trait Component[E <: Entity] {
    def move(dx: Int, dy: Int): Entity
  }
}
