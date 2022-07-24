package eu.joaocosta.roguelike

import scala.annotation.tailrec

import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.rendering.Window

case class GameMap(
    upStairs: (Int, Int),
    downStairs: (Int, Int),
    wallsAndFloor: Map[(Int, Int), GameMap.Tile]
) {

  val tiles: Map[(Int, Int), GameMap.Tile] =
    wallsAndFloor + (upStairs -> GameMap.Tile.UpStairs) + (downStairs -> GameMap.Tile.DownStairs)

  def isTransparent(x: Int, y: Int) = tiles.get((x, y)).forall(_.walkable)

  def visibleFrom(x: Int, y: Int, range: Int): Set[(Int, Int)] = {
    // Don't allow the floodFill to "go back"
    def sameDirection(seedX: Int, seedY: Int, dx: Int, dy: Int): Boolean = {
      val sdx = math.signum(seedX - x)
      val sdy = math.signum(seedY - y)
      (sdx == 0 || dx == 0 || sdx == dx) && (sdy == 0 || dy == 0 || sdy == dy)
    }

    @tailrec
    def floodFill(seeds: Set[((Int, Int), Double)], acc: Set[(Int, Int)]): Set[(Int, Int)] =
      if (seeds.isEmpty) acc
      else {
        val candidates = (for {
          ((sx, sy), remaining) <- seeds
          dx                    <- -1 to 1
          dy                    <- -1 to 1
          if !(dx == 0 && dy == 0) && sameDirection(sx, sy, dx, dy)
          cost          = if (dx == 0 || dy == 0) 1.0 else math.sqrt(2)
          nextRemaining = remaining - cost
          if nextRemaining >= 0
        } yield ((sx + dx, sy + dy), nextRemaining)).toSet
        val nextSeeds = candidates.filter { case ((x, y), _) => isTransparent(x, y) }
        floodFill(nextSeeds, acc ++ candidates.map(_._1))
      }
    floodFill(Set(((x, y), range)), Set((x, y)))
  }

  def isVisibleFrom(x1: Int, y1: Int, x2: Int, y2: Int, range: Int) =
    if (math.abs(x1 - x2) > range || math.abs(y1 - y2) > range) false
    else visibleFrom(x1, y1, range).contains((x2, y2))
}

object GameMap {
  enum Tile(
      val walkable: Boolean,
      val sprite: Window.Sprite,
      val darkSprite: Window.Sprite
  ) {
    case Wall
        extends Tile(
          walkable = false,
          sprite = Window.Sprite('#', Pallete.darkBrown, Pallete.gray),
          darkSprite = Window.Sprite('#', Pallete.black, Pallete.darkGray)
        )
    case Floor
        extends Tile(
          walkable = true,
          sprite = Window.Sprite(' ', Pallete.white, Pallete.gray),
          darkSprite = Window.Sprite(' ', Pallete.gray, Pallete.darkGray)
        )
    case UpStairs
        extends Tile(
          walkable = true,
          sprite = Window.Sprite('<', Pallete.brown, Pallete.gray),
          darkSprite = Window.Sprite('<', Pallete.darkBrown, Pallete.darkGray)
        )
    case DownStairs
        extends Tile(
          walkable = true,
          sprite = Window.Sprite('>', Pallete.darkBlue, Pallete.gray),
          darkSprite = Window.Sprite('>', Pallete.black, Pallete.darkGray)
        )
  }
}
