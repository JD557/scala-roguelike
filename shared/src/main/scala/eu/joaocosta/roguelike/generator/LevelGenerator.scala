package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec
import scala.util.Random

import eu.joaocosta.roguelike._

trait LevelGenerator {
  def generateLevel(random: Random, floor: Int): Level
}

object LevelGenerator {
  case class Distribution[T] private (entries: List[(Int, T)]) {
    val preprocessedEntries: List[(Int, T)] =
      entries.tail.scanLeft(entries.head) { case ((acc, _), (weight, x)) =>
        (acc + weight, x)
      }
    val maxWeight = preprocessedEntries.last._1
    def sample(random: Random): T = {
      val roll = random.nextInt(maxWeight)
      preprocessedEntries.find(_._1 >= roll).getOrElse(preprocessedEntries.last)._2
    }
  }
  object Distribution {
    def apply[T](hd: (Int, T), tl: (Int, T)*): Distribution[T] =
      new Distribution[T](hd :: tl.toList)
  }

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
