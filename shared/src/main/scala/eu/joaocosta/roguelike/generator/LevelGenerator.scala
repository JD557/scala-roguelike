package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec

import eu.joaocosta.roguelike._
import eu.joaocosta.roguelike.generator.Room.RectangularRoom
import eu.joaocosta.roguelike.random.Distribution

trait LevelGenerator {
  def generateLevel(floor: Int): Distribution[Level]
}

object LevelGenerator {

  def filledMap(mapWidth: Int, mapHeight: Int, tile: GameMap.Tile): Map[(Int, Int), GameMap.Tile] =
    (0 until mapWidth).flatMap(x => (0 until mapHeight).map(y => (x, y) -> tile)).toMap

  def positionDistribution(room: Room): Distribution[(Int, Int)] = {
    val hd :: tl = room.tiles.toList
    Distribution.uniform(hd, tl: _*)
  }

  def rectangularRoomDistribution(
      levelWidth: Int,
      levelHeight: Int,
      minWidth: Int,
      maxWidth: Int,
      minHeight: Int,
      maxHeight: Int
  ): Distribution[Room.RectangularRoom] = for {
    roomWidth  <- Distribution.range(minWidth, maxWidth)
    roomHeight <- Distribution.range(minWidth, maxWidth)
    roomX      <- Distribution.range(levelWidth - roomWidth)
    roomY      <- Distribution.range(levelHeight - roomHeight)
  } yield Room.RectangularRoom(roomX, roomY, roomWidth, roomHeight)

  def nonCollidingRoomDistribution[R <: Room](roomDistribution: Distribution[R], maxRooms: Int): Distribution[List[R]] =
    roomDistribution.repeatN(maxRooms).map { rooms =>
      rooms.tails.flatMap {
        case Nil     => None
        case x :: xs => if (xs.exists(_.intersects(x))) None else Some(x)
      }.toList
    }

  val tunnelDistribution: Distribution[(RectangularRoom, RectangularRoom) => Room] = Distribution.uniform(
    (from, to) => Room.TunnelA(from.center._1, from.center._2, to.center._1, to.center._2),
    (from, to) => Room.TunnelB(from.center._1, from.center._2, to.center._1, to.center._2)
  )

}
