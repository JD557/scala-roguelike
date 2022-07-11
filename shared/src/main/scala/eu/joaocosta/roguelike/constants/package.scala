package eu.joaocosta.roguelike

package object constants {
  val screenWidth  = 80
  val screenHeight = 50

  val spriteWidth  = 8
  val spriteHeight = 8

  val playerVision = 8
  val levelGenerator = generator.DefaultLevelGenerator(
    width = 80,
    height = 44,
    roomMaxSize = 10,
    roomMinSize = 6,
    maxRooms = 30,
    maxMonsters = 2,
    maxItems = 1
  )
  val maxMessages = screenHeight - levelGenerator.height - 1
  val hpBarSize   = 20

  val popUpX = hpBarSize
  val popUpW = screenWidth - 2 * popUpX
}
