package eu.joaocosta.roguelike

package object constants {
  val title = "The Minartaur's Lair"

  val screenWidth  = 80
  val screenHeight = 50

  val spriteWidth  = 8
  val spriteHeight = 8

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
  val barSize     = 20

  val popUpX = barSize
  val popUpW = screenWidth - 2 * popUpX - 1

  val statusY      = screenHeight - maxMessages - 1
  val leftStatusX  = 0
  val rightStatusX = popUpX + popUpW + 1

  val playerVision  = 8
  val baseExp       = 200
  val levelUpFactor = 150
  val baseHp        = 30
  val hpBonus       = 20
  val baseAttack    = 5
  val attackBonus   = 1
  val baseDefense   = 1
  val defenseBonus  = 1
}
