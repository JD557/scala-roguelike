package eu.joaocosta.roguelike.entity.components

import eu.joaocosta.roguelike.entity._

case class Inventory(items: List[entities.Item], capacity: Int) {
  val isFull = items.size >= capacity
  def addItem(item: entities.Item): Inventory =
    if (items.size < capacity)
      copy(items = item :: items)
    else this
  def removeItem(item: entities.Item): Inventory = {
    val (left, right) = items.span(_ != item)
    if (right.isEmpty) this
    else copy(items = left ++ right.tail)
  }
}

object Inventory {
  trait Component[E <: Entity with Inventory.Component[E]] {
    def inventory: Inventory
    def updateInventory(f: Inventory => Inventory): E
    def addItem(item: entities.Item): E =
      updateInventory(_.addItem(item))
    def removeItem(item: entities.Item): E =
      updateInventory(_.removeItem(item))
  }
}
