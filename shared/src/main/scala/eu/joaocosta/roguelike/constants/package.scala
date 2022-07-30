package eu.joaocosta.roguelike

import eu.joaocosta.roguelike.entity.entities.Item.FireballScroll
import eu.joaocosta.roguelike.entity.entities.{Equipment, Item, Npc}
import eu.joaocosta.roguelike.random.Distribution

package object constants {
  val title = "The Minartaur's Lair"

  val screenWidth  = 80
  val screenHeight = 50

  val spriteWidth  = 8
  val spriteHeight = 8

  val keyFirstRepeat = 25
  val keyNextRepeat  = 10

  val levelGenerator = generator.DefaultLevelGenerator(
    width = screenWidth,
    height = screenHeight - 7,
    roomMaxSize = 10,
    roomMinSize = 6,
    maxRooms = 30,
    maxMonsters = {
      case floor if floor < 4 => 2
      case floor if floor < 6 => 3
      case _                  => 5
    },
    monsterDistribution = {
      case floor if floor < 3 => Distribution.weighted(80 -> Npc.Orc)
      case floor if floor < 5 => Distribution.weighted(80 -> Npc.Orc, 15 -> Npc.Troll)
      case floor if floor < 7 => Distribution.weighted(80 -> Npc.Orc, 30 -> Npc.Troll)
      case _                  => Distribution.weighted(80 -> Npc.Orc, 60 -> Npc.Troll)
    },
    maxItems = {
      case floor if floor < 4 => 1
      case _                  => 2
    },
    itemDistribution = {
      case floor if floor < 2 => Distribution.weighted(35 -> Item.HealingPotion)
      case floor if floor < 4 =>
        Distribution.weighted(
          35 -> Item.HealingPotion,
          10 -> Item.ConfusionScroll,
          5  -> Equipment.Dagger,
          5  -> Equipment.LeatherArmor
        )
      case floor if floor < 6 =>
        Distribution.weighted(
          35 -> Item.HealingPotion,
          10 -> Item.ConfusionScroll,
          25 -> Item.LightningScroll,
          5  -> Equipment.Sword,
          5  -> Equipment.LeatherArmor
        )
      case _ =>
        Distribution.weighted(
          35 -> Item.HealingPotion,
          10 -> Item.ConfusionScroll,
          25 -> Item.LightningScroll,
          25 -> Item.FireballScroll,
          25 -> Item.LightningScroll,
          15 -> Equipment.Sword,
          15 -> Equipment.ChainMail
        )
    }
  )
  val maxMessages = screenHeight - levelGenerator.height - 2
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
