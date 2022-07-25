package eu.joaocosta.roguelike.entity.components

import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities._
import eu.joaocosta.roguelike.random.Distribution
import eu.joaocosta.roguelike.{Action, Level}

sealed trait Behavior {
  def next(player: Player, level: Level): Distribution[(Behavior, Npc => Action)]
}

object Behavior {
  trait Component[E <: Entity with Behavior.Component[E]] {
    def ai: Behavior
    def updateBehavior(f: Behavior => Behavior): E
  }

  case object DoNothing extends Behavior {
    def next(player: Player, level: Level): Distribution[(Behavior, Npc => Action)] =
      Distribution.pure(this -> (_ => Action.Wait))
  }

  case class JustStare(vision: Int) extends Behavior {
    def next(player: Player, level: Level): Distribution[(Behavior, Npc => Action)] =
      Distribution.pure(this -> ((entity) => {
        if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, vision))
          Action.Stare(entity, player)
        else Action.Wait
      }))
  }

  case class Hostile(vision: Int) extends Behavior {
    def next(player: Player, level: Level): Distribution[(Behavior, Npc => Action)] =
      Distribution.pure(this -> ((entity) => {
        if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, vision)) {
          level.pathfind(entity.x, entity.y, player.x, player.y) match {
            case Nil => Action.Wait
            case (nextX, nextY) :: _ =>
              val dx = nextX - entity.x
              val dy = nextY - entity.y
              Action.Movement(entity, dx, dy)
          }
        } else Action.Wait
      }))
  }

  case object Confused extends Behavior {
    def next(player: Player, level: Level): Distribution[(Behavior, Npc => Action)] =
      Distribution.uniform((-1, 0), (1, 0), (0, -1), (0, 1)).map { case (dx, dy) =>
        this -> ((entity) => Action.Movement(entity, dx, dy))
      }
  }

  case class TemporaryBehavior(oldBehavior: Behavior, currentBehavior: Behavior, remainingTurns: Int) extends Behavior {
    def next(player: Player, level: Level): Distribution[(Behavior, Npc => Action)] =
      if (remainingTurns <= 0) oldBehavior.next(player, level)
      else
        currentBehavior.next(player, level).map { case (nextBehavior, nextAction) =>
          copy(currentBehavior = nextBehavior, remainingTurns = remainingTurns - 1) -> nextAction
        }
  }
}
