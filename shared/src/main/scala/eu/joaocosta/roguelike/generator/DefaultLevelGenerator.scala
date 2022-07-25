package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec
import scala.util.Random

import eu.joaocosta.roguelike._
import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities._
import eu.joaocosta.roguelike.generator.LevelGenerator.Distribution

case class DefaultLevelGenerator(
    width: Int,
    height: Int,
    roomMaxSize: Int,
    roomMinSize: Int,
    maxRooms: Int,
    maxMonsters: Int => Int,
    monsterDistribution: Int => Distribution[(Int, Int) => Npc],
    maxItems: Int => Int,
    itemDistribution: Int => Distribution[(Int, Int) => Item]
) extends LevelGenerator {

  def generateEnemies(room: Room, random: Random, floor: Int): List[Npc] =
    LevelGenerator.randomEntityPositions(room, maxMonsters(floor), random).map { case (x, y) =>
      monsterDistribution(floor).sample(random)(x, y)
    }

  def generateItems(room: Room, random: Random, floor: Int): List[Item] =
    LevelGenerator.randomEntityPositions(room, maxItems(floor), random).map { case (x, y) =>
      itemDistribution(floor).sample(random)(x, y)
    }

  def generateLevel(random: Random, floor: Int): Level = {
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
        (rooms.iterator ++ tunnels.iterator).flatMap(_.tiles).map(pos => pos -> GameMap.Tile.Floor)).toMap
    Level(
      floor = floor,
      gameMap = GameMap(rooms.head.center, rooms.last.center, map),
      entities = rooms.tail.flatMap(room => generateEnemies(room, random, floor) ++ generateItems(room, random, floor))
    )
  }
}
