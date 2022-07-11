package eu.joaocosta.roguelike.constants

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities._

enum Message(val text: String, val color: Color) {
  case Welcome extends Message("Welcome to the Dungeon!", Pallete.blue)
  case Died(target: Entity)
      extends Message(
        s"${target.name} died",
        Message.ifPlayer(target, Pallete.red, Pallete.orange)
      )
  case Attacked(source: Entity, target: Entity, verb: String)
      extends Message(
        s"The ${source.name} ${verb} the ${target.name}",
        Message.ifPlayer(target, Pallete.lightRed, Pallete.white)
      )
  case Damaged(target: Entity, damage: Int)
      extends Message(
        if (damage <= 0) s"It had no effect"
        else s"${target.name} took ${damage} damage",
        if (damage <= 0) Pallete.gray
        else Message.ifPlayer(target, Pallete.lightRed, Pallete.white)
      )
  case Healed(target: Entity, effectiveAmount: Int)
      extends Message(
        s"${target.name} recovered ${effectiveAmount} HP",
        Message.ifPlayer(target, Pallete.green, Pallete.darkGreen)
      )
  case Stare(source: Entity, target: Entity)
      extends Message(s"${source.name} is looking at ${target.name}", Pallete.gray)
  case InventoryFull(source: Entity) extends Message(s"${source.name} can't carry anything else", Pallete.orange)
  case PickedUp(source: Entity, target: Entity)
      extends Message(s"${source.name} picked up ${target.name}", Pallete.white)
  case UsedItem(source: Entity, target: Entity, item: Item)
      extends Message(
        if (source == target) s"${target.name} ${item.consumeVerb} a ${item.name}"
        else s"${source.name} used a ${item.name} on ${target.name}",
        Pallete.gray
      )
  case DroppedItem(source: Entity, item: Entity)
      extends Message(
        s"${source.name} dropped a ${item.name}",
        Pallete.gray
      )
  case NothingHappened extends Message("Nothing happened...", Pallete.gray)
}
object Message {
  private def ifPlayer[T](entity: Entity, whenTrue: T, whenFalse: T) =
    if (entity.isInstanceOf[Player]) whenTrue else whenFalse
}
