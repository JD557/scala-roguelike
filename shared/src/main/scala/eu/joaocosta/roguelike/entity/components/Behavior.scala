package eu.joaocosta.roguelike.entity.components

import scala.util.Random

import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities._
import eu.joaocosta.roguelike.{Action, Level}

sealed trait Behavior {
  def next(player: Player, level: Level, random: Random): (Npc => Action, Behavior)
}

object Behavior {
  trait Component[E <: Entity with Behavior.Component[E]] {
    def ai: Behavior
    def updateBehavior(f: Behavior => Behavior): E
  }

  case object DoNothing extends Behavior {
    def next(player: Player, level: Level, random: Random): (Npc => Action, Behavior) = ((_ => Action.Wait), this)
  }

  case class JustStare(vision: Int) extends Behavior {
    def next(player: Player, level: Level, random: Random): (Npc => Action, Behavior) =
      (
        (entity) => {
          if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, vision))
            Action.Stare(entity, player)
          else Action.Wait
        },
        this
      )
  }

  case class Hostile(vision: Int) extends Behavior {
    def next(player: Player, level: Level, random: Random): (Npc => Action, Behavior) = (
      (entity) => {
        if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, vision)) {
          level.pathfind(entity.x, entity.y, player.x, player.y) match {
            case Nil => Action.Wait
            case (nextX, nextY) :: _ =>
              val dx = nextX - entity.x
              val dy = nextY - entity.y
              Action.Movement(entity, dx, dy)
          }
        } else Action.Wait
      },
      this
    )
  }

  case object Confused extends Behavior {
    def next(player: Player, level: Level, random: Random): (Npc => Action, Behavior) = ((entity: Npc) => {
      val (dx, dy) = random.shuffle(List((-1, 0), (1, 0), (0, -1), (0, 1))).head
      Action.Movement(entity, dx, dy)
    }) -> this
  }

  case class TemporaryBehavior(oldBehavior: Behavior, currentBehavior: Behavior, remainingTurns: Int) extends Behavior {
    def next(player: Player, level: Level, random: Random): (Npc => Action, Behavior) =
      if (remainingTurns <= 0) oldBehavior.next(player, level, random)
      else {
        val (nextAction, nextBehavior) = currentBehavior.next(player, level, random)
        (nextAction, copy(currentBehavior = nextBehavior, remainingTurns = remainingTurns - 1))
      }
  }
}
