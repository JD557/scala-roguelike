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
  val SmallHealingPotion = HealingItem.Builder(
    name = "Small healing potion",
    sprite = Window.Sprite(':', Pallete.lightBlue),
    heal = 2
  )

  val LargeHealingPotion = HealingItem.Builder(
    name = "Large healing potion",
    sprite = Window.Sprite('!', Pallete.lightBlue),
    heal = 6
  )

  val ConfusionScroll = ConfusionSpell.Builder(
    name = "Confusion scroll",
    sprite = Window.Sprite('~', Pallete.green),
    turns = 10
  )

  val LightningScroll = LightningSpell.Builder(
    name = "Lightning scroll",
    sprite = Window.Sprite('~', Pallete.yellow),
    damage = 20,
    maxRange = 5
  )

  val FireballScroll = FireSpell.Builder(
    name = "Fireball scroll",
    sprite = Window.Sprite('~', Pallete.red),
    damage = 12,
    radius = 2
  )

  final case class HealingItem(x: Int, y: Int, name: String, sprite: Window.Sprite, heal: Int) extends Item {
    def setPosition(x: Int, y: Int): HealingItem = copy(x = x, y = y)
    def consumeResult(user: Entity, entities: List[Entity]): Action = user match {
      case entity: FighterEntity => Action.Heal(List(entity), heal)
      case _                     => Action.NothingHappened
    }
  }
  object HealingItem {
    final case class Builder(name: String, sprite: Window.Sprite, heal: Int) extends Entity.Builder[HealingItem] {
      def apply(x: Int, y: Int): HealingItem = HealingItem(x, y, name, sprite, heal)
    }
  }
  final case class ConfusionSpell(x: Int, y: Int, name: String, sprite: Window.Sprite, turns: Int) extends Item {
    def setPosition(x: Int, y: Int): ConfusionSpell =
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
  object ConfusionSpell {
    final case class Builder(name: String, sprite: Window.Sprite, turns: Int) extends Entity.Builder[ConfusionSpell] {
      def apply(x: Int, y: Int): ConfusionSpell = ConfusionSpell(x, y, name, sprite, turns)
    }
  }
  final case class LightningSpell(x: Int, y: Int, name: String, sprite: Window.Sprite, damage: Int, maxRange: Int)
      extends Item {
    def setPosition(x: Int, y: Int): LightningSpell =
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
  object LightningSpell {
    final case class Builder(name: String, sprite: Window.Sprite, damage: Int, maxRange: Int)
        extends Entity.Builder[LightningSpell] {
      def apply(x: Int, y: Int): LightningSpell = LightningSpell(x, y, name, sprite, damage, maxRange)
    }
  }
  final case class FireSpell(x: Int, y: Int, name: String, sprite: Window.Sprite, damage: Int, radius: Int)
      extends Item {
    def setPosition(x: Int, y: Int): FireSpell =
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
  object FireSpell {
    final case class Builder(name: String, sprite: Window.Sprite, damage: Int, radius: Int)
        extends Entity.Builder[FireSpell] {
      def apply(x: Int, y: Int): FireSpell = FireSpell(x, y, name, sprite, damage, radius)
    }
  }
}
