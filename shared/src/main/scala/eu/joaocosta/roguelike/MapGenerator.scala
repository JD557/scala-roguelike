package eu.joaocosta.roguelike

import scala.annotation.tailrec
import scala.util.Random

object MapGenerator {
  def generateMap(
      width: Int,
      height: Int,
      roomMaxSize: Int,
      roomMinSize: Int,
      maxRooms: Int,
      random: Random
  ): (GameMap, Entity.Player) = {
    @tailrec
    def genRooms(rooms: List[RectangularRoom] = Nil, iteration: Int = 0): List[RectangularRoom] = {
      if (iteration >= maxRooms) rooms
      else {
        val roomWidth  = random.between(roomMinSize, roomMaxSize)
        val roomHeight = random.between(roomMinSize, roomMaxSize)
        val roomX      = random.nextInt(width - roomWidth)
        val roomY      = random.nextInt(height - roomHeight)
        val newRoom    = RectangularRoom(roomX, roomY, roomWidth, roomHeight)
        if (rooms.exists(_.intersects(newRoom))) genRooms(rooms, iteration + 1)
        else genRooms(newRoom :: rooms, iteration + 1)
      }
    }
    val rooms = genRooms()
    val tunnels = rooms.sliding(2).flatMap {
      case roomA :: roomB :: _ =>
        if (random.nextBoolean) Some(TunnelA(roomA.center._1, roomA.center._2, roomB.center._1, roomB.center._2))
        else Some(TunnelB(roomA.center._1, roomA.center._2, roomB.center._1, roomB.center._2))
      case _ => None
    }
    val builder = Map.newBuilder[(Int, Int), GameMap.Tile]
    builder ++= (0 until width).flatMap(x => (0 until height).map(y => (x, y) -> GameMap.Tile.Wall))
    builder ++= (
      (rooms.iterator ++ tunnels.iterator).flatMap(_.carveTiles())
    ).map(pos => pos -> GameMap.Tile.Floor)
    (GameMap(builder.result()), Entity.Player(rooms.head.center._1, rooms.head.center._2))
  }

  sealed trait Carver {
    def carveTiles(): Iterator[(Int, Int)]
    def intersects(that: Carver): Boolean = {
      val thisSet = this.carveTiles().toSet
      that.carveTiles().exists(thisSet)
    }
  }

  case class RectangularRoom(x: Int, y: Int, width: Int, height: Int) extends Carver {
    lazy val center = (x + width / 2, y + height / 2)
    def carveTiles(): Iterator[(Int, Int)] =
      for {
        dx <- (1 until width).iterator
        dy <- (1 until height).iterator
      } yield ((x + dx), (y + dy))
  }

  case class TunnelA(x1: Int, y1: Int, x2: Int, y2: Int) extends Carver {
    def carveTiles(): Iterator[(Int, Int)] = {
      val horizontalLine = (x1 to x2 by math.signum(x2 - x1)).iterator.map(x => (x, y1))
      val verticalLine   = (y1 to y2 by math.signum(y2 - y1)).iterator.map(y => (x2, y))
      horizontalLine ++ verticalLine
    }
  }

  case class TunnelB(x1: Int, y1: Int, x2: Int, y2: Int) extends Carver {
    def carveTiles(): Iterator[(Int, Int)] = {
      val verticalLine   = (y1 to y2 by math.signum(y2 - y1)).iterator.map(y => (x1, y))
      val horizontalLine = (x1 to x2 by math.signum(x2 - x1)).iterator.map(x => (x, y2))
      verticalLine ++ horizontalLine
    }
  }
}
