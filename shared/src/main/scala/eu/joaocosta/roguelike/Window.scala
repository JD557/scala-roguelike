package eu.joaocosta.roguelike

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._

case class Window(tiles: Map[(Int, Int), (Char, Color)]) {
  def render(tileset: SpriteSheet): CanvasIO[Unit] = {
    CanvasIO.foreach(tiles) { case ((x, y), (char, color)) =>
      val sprite = tileset.getSprite(char.toInt).map {
        case Color(255, 255, 255) => color
        case c                    => c
      }
      CanvasIO.blit(sprite)(x * Constants.spriteWidth, y * Constants.spriteHeight)
    }
  }
}
