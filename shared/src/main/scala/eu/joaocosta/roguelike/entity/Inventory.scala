package eu.joaocosta.roguelike.entity

case class Inventory(items: List[Entity.Item], capacity: Int) {
  val isFull = items.size >= capacity
  def addItem(item: Entity.Item): Inventory =
    if (items.size < capacity)
      copy(items = item :: items)
    else this
  def removeItem(item: Entity.Item): Inventory = {
    val (left, right) = items.span(_ != item)
    if (right.isEmpty) this
    else copy(items = left ++ right.tail)
  }
}

object Inventory {
  trait Component[E <: Entity with Inventory.Component[E]] {
    def inventory: Inventory
    def updateInventory(f: Inventory => Inventory): E
    def addItem(item: Entity.Item): E =
      updateInventory(_.addItem(item))
    def removeItem(item: Entity.Item): E =
      updateInventory(_.removeItem(item))
  }
}
