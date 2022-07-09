package eu.joaocosta.roguelike

import scala.annotation.tailrec

import eu.joaocosta.roguelike.entity.Entity

case class Level(
    playerStart: Entity.Player,
    gameMap: GameMap,
    entities: List[Entity]
) {

  lazy val npcs: List[Entity.Npc] = entities.collect { case npc: Entity.Npc => npc }

  def isWalkable(x: Int, y: Int) =
    gameMap.tiles.get((x, y)).forall(_.walkable) && !entities.exists(e => !e.isWalkable && x == e.x && y == e.y)

  def updateEntity(from: Entity, to: Option[Entity]) = {
    val withoutEntity = entities.filterNot(_ == from)
    if (withoutEntity.size == entities.size) this
    else copy(entities = to.fold(withoutEntity)(entity => entity :: withoutEntity))
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
