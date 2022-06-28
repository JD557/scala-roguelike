package eu.joaocosta.roguelike

import eu.joaocosta.minart.backend.defaults._
import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.runtime._

object Resources {
  // Gloop Font by Polyduck from https://www.gridsagegames.com/rexpaint/resources.html
  val font = SpriteSheet(Image.loadBmpImage(Resource("gloop.bmp")).get, Constants.spriteWidth, Constants.spriteHeight)

  // Font extended with Kenney Sprites
  val richFont =
    SpriteSheet(Image.loadBmpImage(Resource("gloop_kenney.bmp")).get, Constants.spriteWidth, Constants.spriteHeight)
}
