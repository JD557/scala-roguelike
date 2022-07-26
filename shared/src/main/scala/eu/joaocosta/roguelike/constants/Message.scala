package eu.joaocosta.roguelike.constants

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities._

enum Message(val text: String, val color: Color) {
  case Welcome                       extends Message("Welcome to the Dungeon!", Pallete.blue)
  case NothingHappened               extends Message("Nothing happened...", Pallete.gray)
  case GainedExp(exp: Int)           extends Message(s"You gained ${exp} exp", Pallete.yellow)
  case LevelUp                       extends Message(s"You leveled up!", Pallete.lightBlue)
  case GoDown                        extends Message("You went down the stairs", Pallete.blue)
  case InventoryFull(source: Entity) extends Message(s"${source.name} can't carry anything else", Pallete.orange)
  case EquipedItem(source: Entity, item: Equipment)
      extends Message(s"${source.name} equiped a ${item.name}", Pallete.gray)
  case UnequipedItem(source: Entity, item: Equipment)
      extends Message(s"${source.name} unequiped a ${item.name}", Pallete.gray)
  case UsedItem(source: Entity, item: Entity)    extends Message(s"${source.name} used a ${item.name}", Pallete.gray)
  case DroppedItem(source: Entity, item: Entity) extends Message(s"${source.name} dropped a ${item.name}", Pallete.gray)
  case PickedUp(source: Entity, target: Entity)
      extends Message(s"${source.name} picked up ${target.name}", Pallete.white)
  case Died(target: Entity)
      extends Message(s"${target.name} died", Message.ifPlayer(target, Pallete.red, Pallete.orange))
  case Stare(source: Entity, target: Entity)
      extends Message(s"${source.name} is looking at ${target.name}", Pallete.gray)
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
        if (effectiveAmount <= 0) s"It had no effect"
        else s"${target.name} recovered ${effectiveAmount} HP",
        if (effectiveAmount <= 0) Pallete.gray
        else Message.ifPlayer(target, Pallete.green, Pallete.darkGreen)
      )
}
object Message {
  private def ifPlayer[T](entity: Entity, whenTrue: T, whenFalse: T) =
    if (entity.isInstanceOf[Player]) whenTrue else whenFalse
}
