package eu.joaocosta.roguelike.rendering

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.roguelike.constants

case class Window(tiles: Map[(Int, Int), Window.Sprite]) {

  def addTiles(newTiles: Iterable[((Int, Int), Window.Sprite)]): Window =
    copy(tiles = tiles ++ newTiles)

  def printLine(x: Int, y: Int, string: String, fg: Color = Color(255, 255, 255), bg: Color = Color(0, 0, 0)): Window =
    addTiles(string.zipWithIndex.map { case (char, dx) => (x + dx, y) -> Window.Sprite(char, fg, bg) })

  def putWindow(x: Int, y: Int, window: Window): Window =
    addTiles(window.tiles.map { case ((xx, yy), v) => (xx + x, yy + y) -> v })

  def putBorders(
      x1: Int,
      y1: Int,
      x2: Int,
      y2: Int,
      fg: Color = Color(255, 255, 255),
      bg: Color = Color(0, 0, 0),
      ul: Char = 174,
      ur: Char = 175,
      bl: Char = 190,
      br: Char = 191,
      hLine: Char = 136,
      vLine: Char = 137
  ): Window = {
    val newTiles = for {
      y <- y1 to y2
      x <- x1 to x2
      tile =
        if (y == y1 && x == x1) ul
        else if (y == y1 && x == x2) ur
        else if (y == y2 && x == x1) bl
        else if (y == y2 && x == x2) br
        else if (y == y1 || y == y2) hLine
        else if (x == x1 || x == x2) vLine
        else ' '
    } yield (x, y) -> Window.Sprite(tile, fg, bg)
    addTiles(newTiles)
  }

  def invertColors(x: Int, y: Int): Window = tiles.get((x, y)) match {
    case None         => this
    case Some(sprite) => addTiles(List((x, y) -> sprite.copy(fg = sprite.bg, bg = sprite.fg)))
  }

  def render(tileset: SpriteSheet): CanvasIO[Unit] = {
    CanvasIO.foreach(tiles) { case ((x, y), Window.Sprite(char, fg, bg)) =>
      val sprite = tileset.getSprite(char.toInt).map {
        case Color(255, 255, 255) => fg
        case Color(0, 0, 0)       => bg
        case c                    => c
      }
      CanvasIO.blit(sprite)(x * constants.spriteWidth, y * constants.spriteHeight)
    }
  }
}

object Window {
  val empty: Window = Window(Map.empty)
  case class Sprite(char: Char, fg: Color = Color(255, 255, 255), bg: Color = Color(0, 0, 0))
}
