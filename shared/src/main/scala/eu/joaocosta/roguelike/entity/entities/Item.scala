package eu.joaocosta.roguelike.entity.entities

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.Action
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components._
import eu.joaocosta.roguelike.rendering.Window

sealed trait Item extends Entity with Consumable.Component {
  val isWalkable = true

  def setPosition(x: Int, y: Int): Item
  def pickTarget(source: Entity, validTargets: List[Entity]): Option[Entity]
  def consumeVerb: String
}

object Item {
  case class HealingPotion(x: Int, y: Int, heal: Int = 4) extends Item {
    val name   = "Healing potion"
    val sprite = Window.Sprite('!', Pallete.lightBlue)
    def setPosition(x: Int, y: Int): HealingPotion =
      copy(x = x, y = y)
    def consumeVerb                                                            = "drinked"
    def pickTarget(source: Entity, validTargets: List[Entity]): Option[Entity] = Some(source)
    def consumeResult(target: Entity) = target match {
      case entity: FighterEntity => List(Action.Heal(entity, heal))
      case _                     => List(Action.NothingHappened)
    }
  }
  case class LightningScroll(x: Int, y: Int, damage: Int = 20, maxRange: Int = 5) extends Item {
    val name   = "Lightning scroll"
    val sprite = Window.Sprite('~', Pallete.yellow)
    def setPosition(x: Int, y: Int): LightningScroll =
      copy(x = x, y = y)
    def consumeVerb = "read"
    def pickTarget(source: Entity, validTargets: List[Entity]): Option[Entity] =
      validTargets.iterator
        .filter(_ != source)
        .map(e => e -> ((source.x - e.x) * (source.x - e.x) + (source.y - e.y) * (source.y - e.y)))
        .filter(_._2 < maxRange * maxRange)
        .minByOption(_._2)
        .map(_._1)
    def consumeResult(target: Entity) = target match {
      case entity: FighterEntity => List(Action.Damage(entity, damage))
      case _                     => List(Action.NothingHappened)
    }
  }
}
