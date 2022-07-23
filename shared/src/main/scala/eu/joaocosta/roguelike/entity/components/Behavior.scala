package eu.joaocosta.roguelike.entity.components

import scala.util.Random

import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities._
import eu.joaocosta.roguelike.{Action, Level}

sealed trait Behavior {
  def next(player: Player, level: Level): (Npc => Action, Behavior)
}

object Behavior {
  trait Component[E <: Entity with Behavior.Component[E]] {
    def ai: Behavior
    def updateBehavior(f: Behavior => Behavior): E
  }

  trait SimpleBehavior extends Behavior {
    def nextAction(player: Player, level: Level): Npc => Action
    def next(player: Player, level: Level): (Npc => Action, Behavior) =
      (nextAction(player, level), this)
  }

  case object DoNothing extends SimpleBehavior {
    def nextAction(player: Player, level: Level): Npc => Action = _ => Action.Wait
  }

  case class JustStare(vision: Int) extends SimpleBehavior {
    def nextAction(player: Player, level: Level): Npc => Action =
      entity => {
        if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, vision))
          Action.Stare(entity, player)
        else Action.Wait
      }
  }

  case class Hostile(vision: Int) extends SimpleBehavior {
    def nextAction(player: Player, level: Level): Npc => Action = entity => {
      if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, vision)) {
        level.pathfind(entity.x, entity.y, player.x, player.y) match {
          case Nil => Action.Wait
          case (nextX, nextY) :: _ =>
            val dx = nextX - entity.x
            val dy = nextY - entity.y
            Action.Movement(entity, dx, dy)
        }
      } else Action.Wait
    }
  }

  case class Confused(random: Random) extends SimpleBehavior {
    def nextAction(player: Player, level: Level): Npc => Action = entity => {
      val (dx, dy) = random.shuffle(List((-1, 0), (1, 0), (0, -1), (0, 1))).head
      Action.Movement(entity, dx, dy)
    }
  }

  case class TemporaryBehavior(oldBehavior: Behavior, currentBehavior: Behavior, remainingTurns: Int) extends Behavior {
    def next(player: Player, level: Level): (Npc => Action, Behavior) =
      if (remainingTurns <= 0) oldBehavior.next(player, level)
      else {
        val (nextAction, nextBehavior) = currentBehavior.next(player, level)
        (nextAction, copy(currentBehavior = nextBehavior, remainingTurns = remainingTurns - 1))
      }
  }
}
