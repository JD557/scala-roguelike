package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec
import scala.util.Random

import eu.joaocosta.roguelike._
import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities._
import eu.joaocosta.roguelike.random.Distribution

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

  def generateEntities[E](
      entityDistribution: Distribution[(Int, Int) => E],
      maxEntities: Int
  )(room: Room, random: Random): List[E] =
    LevelGenerator
      .positionDistribution(room)
      .zipWith(entityDistribution) { case ((x, y), entity) => entity(x, y) }
      .sampleUpToN(maxEntities, random)

  def generateLevel(random: Random, floor: Int): Level = {
    val roomDistribution = LevelGenerator.rectangularRoomDistribution(
      width,
      height,
      roomMinSize,
      roomMaxSize,
      roomMinSize,
      roomMaxSize
    )
    @tailrec
    def genRooms(
        rooms: List[Room.RectangularRoom] = Nil,
        tunnels: List[Room] = Nil,
        iteration: Int = 0
    ): (List[Room.RectangularRoom], List[Room]) = {
      if (iteration >= maxRooms) (rooms, tunnels)
      else {
        val newRoom = roomDistribution.sample(random)
        if (rooms.exists(_.intersects(newRoom))) genRooms(rooms, tunnels, iteration + 1)
        else
          rooms match {
            case Nil => genRooms(newRoom :: rooms, tunnels, iteration + 1)
            case prevRoom :: _ =>
              val newTunnel = LevelGenerator.tunnelDistribution.sample(random)(
                newRoom.center._1,
                newRoom.center._2,
                prevRoom.center._1,
                prevRoom.center._2
              )
              genRooms(newRoom :: rooms, newTunnel :: tunnels, iteration + 1)
          }
      }
    }
    val (rooms, tunnels) = genRooms()
    val map =
      ((0 until width).flatMap(x => (0 until height).map(y => (x, y) -> GameMap.Tile.Wall)).iterator ++
        (rooms.iterator ++ tunnels.iterator).flatMap(_.tiles).map(pos => pos -> GameMap.Tile.Floor)).toMap
    val monsterGenerator = generateEntities(monsterDistribution(floor), maxMonsters(floor))
    val itemGenerator    = generateEntities(itemDistribution(floor), maxItems(floor))
    Level(
      floor = floor,
      gameMap = GameMap(rooms.head.center, rooms.last.center, map),
      entities = rooms.tail.flatMap(room => monsterGenerator(room, random) ++ itemGenerator(room, random))
    )
  }
}
