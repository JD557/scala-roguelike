package eu.joaocosta.roguelike.entity.components

import eu.joaocosta.roguelike.entity.entities._
import eu.joaocosta.roguelike.{Action, Level}

sealed trait Behavior {
  def nextAction(entity: Npc, player: Player, level: Level): Action
}

object Behavior {
  trait Component {
    def ai: Behavior
  }

  case object DoNothing extends Behavior {
    def nextAction(entity: Npc, player: Player, level: Level): Action = Action.Wait
  }

  case class JustStare(vision: Int) extends Behavior {
    def nextAction(entity: Npc, player: Player, level: Level): Action =
      if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, vision))
        Action.Stare(entity, player)
      else Action.Wait
  }

  case class Hostile(vision: Int) extends Behavior {
    def nextAction(entity: Npc, player: Player, level: Level): Action =
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
