package eu.joaocosta.roguelike

import scala.annotation.tailrec
import scala.util.Random

trait LevelGenerator {
  def generateLevel(random: Random): Level
}

object LevelGenerator {
  case class DefaultLevelGenerator(
      width: Int,
      height: Int,
      roomMaxSize: Int,
      roomMinSize: Int,
      maxRooms: Int,
      maxMonsters: Int
  ) extends LevelGenerator {

    def generateEnemies(room: Room, random: Random): List[Entity.Npc] =
      room.randomEntityPositions(maxMonsters, random).map { case (x, y) =>
        if (random.nextDouble() < 0.2) Entity.Troll(x, y)
        else Entity.Orc(x, y)
      }

    def generateLevel(random: Random): Level = {
      @tailrec
      def genRooms(
          rooms: List[Room.RectangularRoom] = Nil,
          tunnels: List[Room] = Nil,
          iteration: Int = 0
      ): (List[Room.RectangularRoom], List[Room]) = {
        if (iteration >= maxRooms) (rooms, tunnels)
        else {
          val newRoom =
            Room.randomRectangularRoom(width, height, roomMinSize, roomMaxSize, roomMinSize, roomMaxSize, random)
          if (rooms.exists(_.intersects(newRoom))) genRooms(rooms, tunnels, iteration + 1)
          else
            rooms match {
              case Nil => genRooms(newRoom :: rooms, tunnels, iteration + 1)
              case prevRoom :: _ =>
                val newTunnel = Room.randomTunnel(
                  newRoom.center._1,
                  newRoom.center._2,
                  prevRoom.center._1,
                  prevRoom.center._2,
                  random
                )
                genRooms(newRoom :: rooms, newTunnel :: tunnels, iteration + 1)
            }
        }
      }
      val (rooms, tunnels) = genRooms()
      val map =
        ((0 until width).flatMap(x => (0 until height).map(y => (x, y) -> GameMap.Tile.Wall)).iterator ++
          (rooms.iterator ++ tunnels.iterator).flatMap(_.tiles()).map(pos => pos -> GameMap.Tile.Floor)).toMap
      Level(
        playerStart = Entity.Player(rooms.head.center._1, rooms.head.center._2),
        gameMap = GameMap(map),
        npcs = rooms.tail.flatMap(room => generateEnemies(room, random))
      )
    }
  }

  sealed trait Room {
    def tiles(): Iterator[(Int, Int)]
    def intersects(that: Room): Boolean = {
      val thisSet = this.tiles().toSet
      that.tiles().exists(thisSet)
    }
    def randomEntityPositions(max: Int, random: Random): List[(Int, Int)] =
      random.shuffle(tiles()).take(random.nextInt(max + 1)).toList
  }

  object Room {
    def randomRectangularRoom(
        levelWidth: Int,
        levelHeight: Int,
        minWidth: Int,
        maxWidth: Int,
        minHeight: Int,
        maxHeight: Int,
        random: Random
    ): RectangularRoom = {
      val roomWidth  = random.between(minWidth, maxWidth)
      val roomHeight = random.between(minWidth, maxWidth)
      val roomX      = random.nextInt(levelWidth - roomWidth)
      val roomY      = random.nextInt(levelHeight - roomHeight)
      RectangularRoom(roomX, roomY, roomWidth, roomHeight)
    }

    def randomTunnel(x1: Int, y1: Int, x2: Int, y2: Int, random: Random): Room = {
      if (random.nextBoolean) TunnelA(x1, y1, x2, y2)
      else TunnelB(x1, y1, x2, y2)
    }

    case class RectangularRoom(x: Int, y: Int, width: Int, height: Int) extends Room {
      lazy val center = (x + width / 2, y + height / 2)
      def tiles(): Iterator[(Int, Int)] =
        for {
          dx <- (1 until width).iterator
          dy <- (1 until height).iterator
        } yield ((x + dx), (y + dy))
    }

    case class TunnelA(x1: Int, y1: Int, x2: Int, y2: Int) extends Room {
      def tiles(): Iterator[(Int, Int)] = {
        val horizontalLine = (x1 to x2 by math.signum(x2 - x1)).iterator.map(x => (x, y1))
        val verticalLine   = (y1 to y2 by math.signum(y2 - y1)).iterator.map(y => (x2, y))
        horizontalLine ++ verticalLine
      }
    }

    case class TunnelB(x1: Int, y1: Int, x2: Int, y2: Int) extends Room {
      def tiles(): Iterator[(Int, Int)] = {
        val verticalLine   = (y1 to y2 by math.signum(y2 - y1)).iterator.map(y => (x1, y))
        val horizontalLine = (x1 to x2 by math.signum(x2 - x1)).iterator.map(x => (x, y2))
        verticalLine ++ horizontalLine
      }
    }
  }
}
