package eu.joaocosta.roguelike.generator

import scala.annotation.tailrec

import eu.joaocosta.roguelike._
import eu.joaocosta.roguelike.entity.Entity
import eu.joaocosta.roguelike.entity.entities._
import eu.joaocosta.roguelike.generator.Room.RectangularRoom
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
  )(room: Room): Distribution[List[E]] =
    LevelGenerator
      .positionDistribution(room)
      .zipWith(entityDistribution) { case ((x, y), entity) => entity(x, y) }
      .repeatUpToN(maxEntities)

  def generateTunnels(rooms: List[RectangularRoom]): Distribution[List[Room]] =
    Distribution.sequence(
      (rooms.init zip rooms.tail).map { case (from, to) => LevelGenerator.tunnelDistribution.map(_(from, to)) }
    )

  val roomDistribution = LevelGenerator.nonCollidingRoomDistribution(
    LevelGenerator.rectangularRoomDistribution(
      width,
      height,
      roomMinSize,
      roomMaxSize,
      roomMinSize,
      roomMaxSize
    ),
    maxRooms
  )

  def generateLevel(floor: Int): Distribution[Level] = {
    val monsterGenerator = generateEntities(monsterDistribution(floor), maxMonsters(floor))
    val itemGenerator    = generateEntities(itemDistribution(floor), maxItems(floor))
    for {
      rooms   <- roomDistribution
      tunnels <- generateTunnels(rooms)
      map = LevelGenerator.filledMap(width, height, GameMap.Tile.Wall) ++
        (rooms.iterator ++ tunnels.iterator).flatMap(_.tiles).map(pos => pos -> GameMap.Tile.Floor).toMap
      gameMap = GameMap(rooms.head.center, rooms.last.center, map)
      entities <- Distribution
        .traverse(rooms.tail)(room => monsterGenerator(room).zipWith(itemGenerator(room))(_ ++ _))
        .map(_.flatten)
    } yield Level(
      floor = floor,
      gameMap = gameMap,
      entities = entities
    )
  }
}
