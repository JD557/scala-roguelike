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
      case entity: FighterEntity => Action.Heal(List(entity), heal)
      case _                     => Action.NothingHappened
    }
  }
  case class LightningScroll(x: Int, y: Int, damage: Int = 20, maxRange: Int = 5) extends Item {
    val name   = "Lightning scroll"
    val sprite = Window.Sprite('~', Pallete.yellow)
    def setPosition(x: Int, y: Int): LightningScroll =
      copy(x = x, y = y)
    def consumeResult(user: Entity, entities: List[Entity]): Action = {
      def distance(x: Int, y: Int): Int = {
        val dx = (user.x - x)
        val dy = (user.y - y)
        (dx * dx + dy * dy)
      }
      val maxDistance = maxRange * maxRange
      entities.iterator
        .foldLeft[Option[(FighterEntity, Int)]](None) {
          case (closestEntity, e1: FighterEntity) =>
            val dist1 = distance(e1.x, e1.y)
            if (dist1 < maxRange) closestEntity.map { case (e2, dist2) =>
              if (dist1 < dist2) (e1, dist1) else (e2, dist2)
            }
            else closestEntity
          case (closestEntity, _) => closestEntity
        }
        .fold(Action.NothingHappened) { case (entity, _) => Action.Damage(List(entity), damage) }
    }
  }
  case class FireballScroll(x: Int, y: Int, damage: Int = 12, radius: Int = 2) extends Item {
    val name   = "Fireball scroll"
    val sprite = Window.Sprite('~', Pallete.red)
    def setPosition(x: Int, y: Int): FireballScroll =
      copy(x = x, y = y)
    def consumeResult(user: Entity, entities: List[Entity]): Action = {
      Action.LookAround(
        { selectedEntities =>
          val fighters = selectedEntities.collect { case e: FighterEntity => e }
          if (fighters.isEmpty) Action.NothingHappened
          else Action.Damage(fighters, damage)
        },
        radius
      )
    }
  }
  case class ConfusionScroll(x: Int, y: Int, turns: Int = 10) extends Item {
    val name   = "Confusion scroll"
    val sprite = Window.Sprite('~', Pallete.green)
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
