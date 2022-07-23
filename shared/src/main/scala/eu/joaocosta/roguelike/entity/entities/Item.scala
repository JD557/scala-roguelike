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
}

object Item {
  case class HealingPotion(x: Int, y: Int, heal: Int = 4) extends Item {
    val name   = "Healing potion"
    val sprite = Window.Sprite('!', Pallete.lightBlue)
    def setPosition(x: Int, y: Int): HealingPotion =
      copy(x = x, y = y)
    def consumeResult(user: Entity, entities: List[Entity]): Action = user match {
      case entity: FighterEntity => Action.Heal(entity, heal)
      case _                     => Action.NothingHappened
    }
  }
  case class LightningScroll(x: Int, y: Int, damage: Int = 20, maxRange: Int = 5) extends Item {
    val name   = "Lightning scroll"
    val sprite = Window.Sprite('~', Pallete.yellow)
    def setPosition(x: Int, y: Int): LightningScroll =
      copy(x = x, y = y)
    def consumeResult(user: Entity, entities: List[Entity]): Action = {
      entities.iterator
        .collect {
          case (entity: FighterEntity) if entity != user =>
            val dx = (user.x - entity.x)
            val dy = (user.y - entity.y)
            (entity, (dx * dx + dy * dy))
        }
        .filter(_._2 < maxRange * maxRange)
        .minByOption(_._2)
        .map { case (entity, _) => Action.Damage(entity, damage) }
        .getOrElse(Action.NothingHappened)
    }
  }
  case class ConfusionScroll(x: Int, y: Int, turns: Int = 5) extends Item {
    val name   = "Confusion scroll"
    val sprite = Window.Sprite('?', Pallete.yellow)
    def setPosition(x: Int, y: Int): ConfusionScroll =
      copy(x = x, y = y)
    def consumeResult(user: Entity, entities: List[Entity]): Action =
      Action.LookAround { selectedEntities =>
        selectedEntities
          .collectFirst { case e: BehaviorEntity =>
            Action.ChangeBehavior(
              e,
              oldBehavior => Behavior.TemporaryBehavior(oldBehavior, Behavior.Confused, turns)
            )
          }
          .getOrElse(Action.NothingHappened)
      }
  }
}
