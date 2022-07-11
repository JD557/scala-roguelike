package eu.joaocosta.roguelike.entity

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.rendering.Window
import eu.joaocosta.roguelike.{Action, Constants}

sealed trait Entity {
  def x: Int
  def y: Int

  def name: String
  def sprite: Window.Sprite
  def isWalkable: Boolean
}

object Entity {
  case class Player(
      x: Int,
      y: Int,
      fighter: Fighter = Fighter(30, 30, 5, 2, "kicked"),
      inventory: Inventory = Inventory(Nil, 26)
  ) extends Entity
      with Moveable.Component[Player]
      with Fighter.Component[Player]
      with Inventory.Component[Player] {
    val name = "Player"
    val sprite =
      if (fighter.isDead) Window.Sprite('%', Constants.Pallete.white)
      else Window.Sprite('@', Constants.Pallete.white)
    val isWalkable = false

    def move(dx: Int, dy: Int): Player                     = copy(x = x + dx, y = y + dy)
    def updateFighter(f: Fighter => Fighter): Player       = copy(fighter = f(fighter))
    def updateInventory(f: Inventory => Inventory): Player = copy(inventory = f(inventory))
  }

  sealed trait Npc extends Entity with Moveable.Component[Npc] with Fighter.Component[Npc] with Behavior.Component {
    val isWalkable = false
  }
  case class Orc(x: Int, y: Int, fighter: Fighter = Fighter(10, 10, 3, 0, "punched")) extends Npc {
    val name   = "Orc"
    val sprite = Window.Sprite('o', Constants.Pallete.darkGreen)

    def move(dx: Int, dy: Int): Orc               = copy(x = x + dx, y = y + dy)
    def updateFighter(f: Fighter => Fighter): Orc = copy(fighter = f(fighter))
    val ai                                        = Behavior.JustStare
  }
  case class Troll(x: Int, y: Int, fighter: Fighter = Fighter(16, 16, 4, 1, "clubed")) extends Npc {
    val name   = "Troll"
    val sprite = Window.Sprite('T', Constants.Pallete.green)
    val ai     = Behavior.Hostile

    def move(dx: Int, dy: Int): Troll               = copy(x = x + dx, y = y + dy)
    def updateFighter(f: Fighter => Fighter): Troll = copy(fighter = f(fighter))
  }
  case class Corpse(of: Entity) extends Entity {
    val name       = s"${of.name} corpse"
    val sprite     = Window.Sprite('%', Constants.Pallete.darkGray)
    val isWalkable = true

    val x = of.x
    val y = of.y
  }
  sealed trait Item extends Entity with Consumable.Component {
    val isWalkable = true

    def setPosition(x: Int, y: Int): Item
    def pickTarget(source: Entity, validTargets: List[Entity]): Option[Entity]
    def consumeVerb: String
  }
  case class HealingPotion(x: Int, y: Int, heal: Int = 4) extends Item {
    val name   = "Healing potion"
    val sprite = Window.Sprite('!', Constants.Pallete.lightBlue)
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
    val sprite = Window.Sprite('~', Constants.Pallete.yellow)
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
