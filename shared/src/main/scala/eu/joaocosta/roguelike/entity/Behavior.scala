package eu.joaocosta.roguelike.entity

import eu.joaocosta.roguelike.{Action, Constants, Level}

sealed trait Behavior {
  def nextAction(entity: Entity.Npc, player: Entity.Player, level: Level): Action
}

object Behavior {
  trait Component {
    def ai: Behavior
  }

  case object DoNothing extends Behavior {
    def nextAction(entity: Entity.Npc, player: Entity.Player, level: Level): Action = Action.Wait
  }

  case object JustStare extends Behavior {
    def nextAction(entity: Entity.Npc, player: Entity.Player, level: Level): Action =
      if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, Constants.playerVision))
        Action.Stare(entity, player)
      else Action.Wait
  }

  case object Hostile extends Behavior {
    def nextAction(entity: Entity.Npc, player: Entity.Player, level: Level): Action =
      if (level.gameMap.isVisibleFrom(entity.x, entity.y, player.x, player.y, Constants.playerVision)) {
        level.pathfind(entity.x, entity.y, player.x, player.y) match {
          case Nil => Action.Wait
          case (nextX, nextY) :: _ =>
            val dx = nextX - entity.x
            val dy = nextY - entity.y
            Action.NpcMovement(entity, dx, dy)
        }
      } else Action.Wait
  }
}
