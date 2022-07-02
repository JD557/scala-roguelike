package eu.joaocosta.roguelike

import scala.annotation.tailrec

import eu.joaocosta.roguelike.entity.Entity

case class Level(
    playerStart: Entity.Player,
    gameMap: GameMap,
    npcs: List[Entity.Npc]
) {
  def isWalkable(x: Int, y: Int) =
    gameMap.tiles.get((x, y)).forall(_.walkable) && !npcs.exists(npc => x == npc.x && y == npc.y)

  def updateNpc(from: Entity.Npc, to: Option[Entity.Npc]) = {
    val withoutNpc = npcs.filterNot(_ == from)
    if (withoutNpc.size == npcs.size) this
    else copy(npcs = to.fold(withoutNpc)(npc => npc :: withoutNpc))
  }

  def pathfind(x1: Int, y1: Int, x2: Int, y2: Int): List[(Int, Int)] = {
    @tailrec
    def genPaths(candidates: Set[(Int, Int)], paths: Map[(Int, Int), (Int, Int)]): Map[(Int, Int), (Int, Int)] = {
      val newPaths = candidates.flatMap { case (x, y) =>
        Set((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
          .filterNot(paths.contains)
          .filter(isWalkable)
          .map(dest => dest -> (x, y))
      }.toMap
      val nextCandidates = newPaths.keySet
      val nextPaths      = paths ++ newPaths
      if (nextCandidates.contains((x2, y2))) nextPaths
      else genPaths(nextCandidates, nextPaths)
    }
    val paths = genPaths(candidates = Set((x1, y1)), paths = Map.empty)
    def buildPath(curr: (Int, Int), acc: List[(Int, Int)]): List[(Int, Int)] = {
      paths.get(curr) match {
        case None                           => Nil
        case Some(next) if next == (x1, y1) => curr :: acc
        case Some(next)                     => buildPath(next, curr :: acc)
      }
    }
    buildPath((x2, y2), Nil)
  }
}
