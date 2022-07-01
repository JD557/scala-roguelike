package eu.joaocosta.roguelike

import scala.annotation.tailrec

case class GameMap(tiles: Map[(Int, Int), GameMap.Tile]) {
  def isTransparent(x: Int, y: Int) = tiles.get((x, y)).forall(_.walkable)

  def visibleFrom(x: Int, y: Int, range: Int): Set[(Int, Int)] = {
    @tailrec
    def floodFill(seeds: Set[(Int, Int)], remaining: Int, acc: Set[(Int, Int)]): Set[(Int, Int)] =
      if (remaining <= 0 || seeds.isEmpty) acc
      else {
        val candidates = (for {
          (sx, sy) <- seeds
          dx       <- -1 to 1
          dy       <- -1 to 1
        } yield (sx + dx, sy + dy)).toSet
        val nextSeeds = candidates.filter(isTransparent)
        floodFill(nextSeeds, remaining - 1, acc ++ candidates)
      }
    floodFill(Set((x, y)), range, Set((x, y)))
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
          sprite = Window.Sprite('#', Constants.Pallete.red, Constants.Pallete.orange),
          darkSprite = Window.Sprite('#', Constants.Pallete.darkGray, Constants.Pallete.gray)
        )
    case Floor
        extends Tile(
          walkable = true,
          sprite = Window.Sprite(' ', Constants.Pallete.orange, Constants.Pallete.orange),
          darkSprite = Window.Sprite(' ', Constants.Pallete.gray, Constants.Pallete.gray)
        )
  }
}
