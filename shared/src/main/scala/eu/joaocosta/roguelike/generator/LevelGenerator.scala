package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec
import scala.util.Random

import eu.joaocosta.roguelike._

trait LevelGenerator {
  def generateLevel(random: Random, floor: Int): Level
}

object LevelGenerator {

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

  val tunnelDistribution: Distribution[(Int, Int, Int, Int) => Room] = Distribution.uniform(
    Room.TunnelA(_, _, _, _),
    Room.TunnelB(_, _, _, _)
  )
}
