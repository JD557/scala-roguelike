package eu.joaocosta.roguelike.entity

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.Action
import eu.joaocosta.roguelike.rendering.Window

trait Entity {
  def x: Int
  def y: Int

  def name: String
  def sprite: Window.Sprite
  def isWalkable: Boolean
  def setPosition(x: Int, y: Int): Entity
}

object Entity {
  type Builder[E <: Entity] = Function2[Int, Int, E]
}
