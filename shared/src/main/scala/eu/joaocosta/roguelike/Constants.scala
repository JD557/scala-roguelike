package eu.joaocosta.roguelike

import eu.joaocosta.minart.graphics._
import eu.joaocosta.roguelike.entity.Entity

object Constants {
  val screenWidth  = 80
  val screenHeight = 50

  val spriteWidth  = 8
  val spriteHeight = 8

  val playerVision = 8
  val levelGenerator = generator.DefaultLevelGenerator(
    width = 80,
    height = 45,
    roomMaxSize = 10,
    roomMinSize = 6,
    maxRooms = 30,
    maxMonsters = 2
  )
  val maxMessages = screenHeight - levelGenerator.height - 1
  val hpBarSize   = 20

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

  enum Message(val text: String, val color: Color) {
    case Welcome extends Message("Welcome to the Dungeon!", Constants.Pallete.blue)
    case Killed(source: Entity, target: Entity)
        extends Message(
          s"The ${source.name} killed the ${target.name}",
          Message.ifPlayer(target, Constants.Pallete.red, Constants.Pallete.orange)
        )
    case Damaged(source: Entity, target: Entity, damage: Int)
        extends Message(
          s"The ${source.name} kicked the ${target.name} for ${damage} damage",
          Message.ifPlayer(target, Constants.Pallete.lightRed, Constants.Pallete.white)
        )
    case Healed(target: Entity, effectiveAmount: Int)
        extends Message(
          s"${target.name} recovered ${effectiveAmount} HP",
          Message.ifPlayer(target, Constants.Pallete.green, Constants.Pallete.darkGreen)
        )
    case Stare(source: Entity, target: Entity)
        extends Message(s"${source.name} is looking at ${target.name}", Constants.Pallete.gray)
    case NothingHappened extends Message("Nothing happened...", Constants.Pallete.gray)
  }
  object Message {
    private def ifPlayer[T](entity: Entity, whenTrue: T, whenFalse: T) =
      if (entity.isInstanceOf[Entity.Player]) whenTrue else whenFalse
  }
}
