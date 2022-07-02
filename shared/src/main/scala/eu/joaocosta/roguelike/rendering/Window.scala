package eu.joaocosta.roguelike.rendering

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.roguelike.Constants

case class Window(tiles: Map[(Int, Int), Window.Sprite]) {

  def addTiles(newTiles: Iterable[((Int, Int), Window.Sprite)]): Window =
    copy(tiles = tiles ++ newTiles)

  def printLine(x: Int, y: Int, string: String, fg: Color = Color(255, 255, 255), bg: Color = Color(0, 0, 0)): Window =
    addTiles(string.zipWithIndex.map { case (char, dx) => (x + dx, y) -> Window.Sprite(char, fg, bg) })

  def render(tileset: SpriteSheet): CanvasIO[Unit] = {
    CanvasIO.foreach(tiles) { case ((x, y), Window.Sprite(char, fg, bg)) =>
      val sprite = tileset.getSprite(char.toInt).map {
        case Color(255, 255, 255) => fg
        case Color(0, 0, 0)       => bg
        case c                    => c
      }
      CanvasIO.blit(sprite)(x * Constants.spriteWidth, y * Constants.spriteHeight)
    }
  }
}

object Window {
  val empty: Window = Window(Map.empty)
  case class Sprite(char: Char, fg: Color = Color(255, 255, 255), bg: Color = Color(0, 0, 0))
}
