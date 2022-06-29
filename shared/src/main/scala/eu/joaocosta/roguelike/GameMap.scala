package eu.joaocosta.roguelike

case class GameMap(tiles: Map[(Int, Int), GameMap.Tile]) {
  def isWalkable(x: Int, y: Int) = tiles.get((x, y)).forall(_.walkable)
}

object GameMap {
  enum Tile(val walkable: Boolean, val sprite: Window.Sprite) {
    case Wall  extends Tile(walkable = false, Window.Sprite(' ', Constants.Pallete.gray, Constants.Pallete.gray))
    case Floor extends Tile(true, Window.Sprite(' ', Constants.Pallete.black, Constants.Pallete.black))
  }
}
