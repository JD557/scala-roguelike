package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec
import scala.util.Random

import eu.joaocosta.roguelike._

trait LevelGenerator {
  def generateLevel(random: Random): Level
}

object LevelGenerator {
  def randomEntityPositions(room: Room, maxEntities: Int, random: Random): List[(Int, Int)] =
    random.shuffle(room.tiles.iterator).take(random.nextInt(maxEntities + 1)).toList

  def randomRectangularRoom(
      levelWidth: Int,
      levelHeight: Int,
      minWidth: Int,
      maxWidth: Int,
      minHeight: Int,
      maxHeight: Int,
      random: Random
  ): Room.RectangularRoom = {
    val roomWidth  = random.between(minWidth, maxWidth)
    val roomHeight = random.between(minWidth, maxWidth)
    val roomX      = random.nextInt(levelWidth - roomWidth)
    val roomY      = random.nextInt(levelHeight - roomHeight)
    Room.RectangularRoom(roomX, roomY, roomWidth, roomHeight)
  }

  def randomTunnel(x1: Int, y1: Int, x2: Int, y2: Int, random: Random): Room = {
    if (random.nextBoolean) Room.TunnelA(x1, y1, x2, y2)
    else Room.TunnelB(x1, y1, x2, y2)
  }
}
