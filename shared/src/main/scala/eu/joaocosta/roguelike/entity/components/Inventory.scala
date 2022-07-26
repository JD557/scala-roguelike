package eu.joaocosta.roguelike.entity.components

import eu.joaocosta.roguelike.entity._

case class Inventory(items: List[ConsumableEntity], capacity: Int) {
  val isFull = items.size >= capacity
  def addItem(item: ConsumableEntity): Inventory =
    if (!isFull) copy(items = item :: items)
    else this
  def removeItem(item: Entity): Inventory = {
    val (left, right) = items.span(_ != item)
    if (right.isEmpty) this
    else copy(items = left ++ right.tail)
  }
}

object Inventory {
  trait Component[E <: Entity with Inventory.Component[E]] {
    def inventory: Inventory
    def updateInventory(f: Inventory => Inventory): E
    def addItem(item: ConsumableEntity): E =
      updateInventory(_.addItem(item))
    def removeItem(item: Entity): E =
      updateInventory(_.removeItem(item))
  }
}
