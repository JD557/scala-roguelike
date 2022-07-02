package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec
import scala.util.Random

import eu.joaocosta.roguelike._

case class DefaultLevelGenerator(
    width: Int,
    height: Int,
    roomMaxSize: Int,
    roomMinSize: Int,
    maxRooms: Int,
    maxMonsters: Int
) extends LevelGenerator {

  def generateEnemies(room: Room, random: Random): List[Entity.Npc] =
    LevelGenerator.randomEntityPositions(room, maxMonsters, random).map { case (x, y) =>
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
          LevelGenerator.randomRectangularRoom(
            width,
            height,
            roomMinSize,
            roomMaxSize,
            roomMinSize,
            roomMaxSize,
            random
          )
        if (rooms.exists(_.intersects(newRoom))) genRooms(rooms, tunnels, iteration + 1)
        else
          rooms match {
            case Nil => genRooms(newRoom :: rooms, tunnels, iteration + 1)
            case prevRoom :: _ =>
              val newTunnel = LevelGenerator.randomTunnel(
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