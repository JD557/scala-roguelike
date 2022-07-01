package eu.joaocosta.roguelike

import eu.joaocosta.minart.graphics._

object Constants {
  val screenWidth  = 80
  val screenHeight = 50

  val spriteWidth  = 8
  val spriteHeight = 8

  val playerVision = 5

  object Pallete { // From http://androidarts.com/palette/16pal.htm
    val black      = Color(0, 0, 0)
    val gray       = Color(157, 157, 157)
    val white      = Color(255, 255, 255)
    val red        = Color(190, 38, 51)
    val lightRed   = Color(190, 38, 51)
    val darkBrown  = Color(73, 60, 43)
    val brown      = Color(164, 100, 34)
    val orange     = Color(235, 137, 49)
    val yellow     = Color(247, 226, 107)
    val darkGreen  = Color(47, 42, 78)
    val green      = Color(68, 137, 26)
    val lightGreen = Color(163, 206, 39)
    val darkGray   = Color(27, 38, 50)
    val darkBlue   = Color(0, 87, 132)
    val blue       = Color(49, 162, 242)
    val lightBlue  = Color(178, 220, 239)
  }
}
