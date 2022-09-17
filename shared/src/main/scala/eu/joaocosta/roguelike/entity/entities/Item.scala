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
    heal = 3
  )

  val LargeHealingPotion = HealingItem.Builder(
    name = "Large healing potion",
    sprite = Window.Sprite('!', Pallete.lightBlue),
    heal = 6
  )

  val ConfusionScroll = StatusSpell.Builder(
    name = "Confusion scroll",
    sprite = Window.Sprite('~', Pallete.green),
    behavior = Behavior.Confused,
    turns = 10
  )

  val ParalysisTome = StatusSpell.Builder(
    name = "Paralysis tome",
    sprite = Window.Sprite('=', Pallete.green),
    behavior = Behavior.DoNothing,
    turns = 5
  )

  val LightningScroll = LightningSpell.Builder(
    name = "Lightning scroll",
    sprite = Window.Sprite('~', Pallete.yellow),
    damage = 20,
    maxRange = 8
  )

  val LightningTome = LightningSpell.Builder(
    name = "Thunderstorm tome",
    sprite = Window.Sprite('=', Pallete.yellow),
    damage = 40,
    maxRange = 8
  )

  val FireballScroll = FireSpell.Builder(
    name = "Fireball scroll",
    sprite = Window.Sprite('~', Pallete.red),
    damage = 12,
    radius = 2
  )

  val FireballTome = FireSpell.Builder(
    name = "Hellfire tome",
    sprite = Window.Sprite('=', Pallete.red),
    damage = 20,
    radius = 4
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

  final case class StatusSpell(x: Int, y: Int, name: String, sprite: Window.Sprite, behavior: Behavior, turns: Int)
      extends Item {
    def setPosition(x: Int, y: Int): StatusSpell =
      copy(x = x, y = y)
    def consumeResult(user: Entity, entities: List[Entity]): Action =
      Action.LookAround { selectedEntities =>
        selectedEntities
          .collectFirst { case e: BehaviorEntity =>
            Action.ChangeBehavior(
              e,
              oldBehavior => Behavior.TemporaryBehavior(oldBehavior, behavior, turns)
            )
          }
          .getOrElse(Action.NothingHappened)
      }
  }
  object StatusSpell {
    final case class Builder(name: String, sprite: Window.Sprite, behavior: Behavior, turns: Int)
        extends Entity.Builder[StatusSpell] {
      def apply(x: Int, y: Int): StatusSpell = StatusSpell(x, y, name, sprite, behavior, turns)
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
        .collect { case e: FighterEntity =>
          (e, distance(e.x, e.y))
        }
        .foldLeft[Option[(FighterEntity, Int)]](None) {
          case (closestEntity, (e1, dist1)) if dist1 != 0 =>
            val currentEntity = Some((e1, dist1))
            closestEntity match {
              case None if dist1 < maxDistance          => currentEntity
              case Some((e2, dist2)) if (dist1 < dist2) => currentEntity
              case _                                    => closestEntity
            }
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
