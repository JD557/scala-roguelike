package eu.joaocosta.roguelike

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._

case class Window(tiles: Map[(Int, Int), Window.Sprite]) {
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
  case class Sprite(char: Char, fg: Color = Color(255, 255, 255), bg: Color = Color(0, 0, 0))
}
