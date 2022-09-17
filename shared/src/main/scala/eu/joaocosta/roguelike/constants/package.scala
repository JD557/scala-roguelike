package eu.joaocosta.roguelike

import eu.joaocosta.roguelike.entity.entities.{Equipment, Item, Npc}
import eu.joaocosta.roguelike.random.Distribution

package object constants {
  val title = "The Minartaur's Lair"

  val screenWidth  = 80
  val screenHeight = 50

  val spriteWidth  = 8
  val spriteHeight = 8

  val keyFirstRepeat = 25
  val keyNextRepeat  = 7

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
      case floor if floor < 2  => Distribution.weighted(40 -> Npc.Spider, 40 -> Npc.Bat, 20 -> Npc.Goblin)
      case floor if floor < 3  => Distribution.weighted(25 -> Npc.Spider, 25 -> Npc.Bat, 50 -> Npc.Goblin)
      case floor if floor < 5  => Distribution.weighted(10 -> Npc.Bat, 80 -> Npc.Goblin, 10 -> Npc.Orc)
      case floor if floor < 7  => Distribution.weighted(70 -> Npc.Goblin, 30 -> Npc.Orc)
      case floor if floor < 10 => Distribution.weighted(50 -> Npc.Goblin, 40 -> Npc.Orc, 10 -> Npc.Troll)
      case floor if floor < 12 =>
        Distribution.weighted(20 -> Npc.Goblin, 40 -> Npc.Orc, 20 -> Npc.Troll, 14 -> Npc.Centaur, 1 -> Npc.Minartaur)
      case _ => Distribution.weighted(10 -> Npc.Orc, 40 -> Npc.Troll, 45 -> Npc.Centaur, 5 -> Npc.Minartaur)
    },
    maxItems = {
      case floor if floor < 4 => 1
      case _                  => 2
    },
    itemDistribution = {
      case floor if floor < 2 =>
        Distribution.weighted(
          100 -> Item.SmallHealingPotion
        )
      case floor if floor < 4 =>
        Distribution.weighted(
          65 -> Item.SmallHealingPotion,
          5  -> Item.LargeHealingPotion,
          10 -> Item.ConfusionScroll,
          5  -> Item.LightningScroll,
          15 -> Equipment.Club
        )
      case floor if floor < 6 =>
        Distribution.weighted(
          40 -> Item.SmallHealingPotion,
          10 -> Item.LargeHealingPotion,
          10 -> Item.ConfusionScroll,
          10 -> Item.LightningScroll,
          10 -> Equipment.Club,
          5  -> Equipment.Dagger,
          5  -> Equipment.LeatherArmor
        )
      case floor if floor < 10 =>
        Distribution.weighted(
          20 -> Item.SmallHealingPotion,
          20 -> Item.LargeHealingPotion,
          15 -> Item.ConfusionScroll,
          15 -> Item.LightningScroll,
          5  -> Item.FireballScroll,
          10 -> Equipment.Dagger,
          5  -> Equipment.Sword,
          10 -> Equipment.LeatherArmor,
          5  -> Equipment.ChainMail
        )
      case _ =>
        Distribution.weighted(
          30 -> Item.LargeHealingPotion,
          9  -> Item.ConfusionScroll,
          1  -> Item.ParalysisTome,
          14 -> Item.LightningScroll,
          1  -> Item.LightningTome,
          14 -> Item.FireballScroll,
          1  -> Item.FireballTome,
          5  -> Equipment.Dagger,
          10 -> Equipment.Sword,
          10 -> Equipment.ChainMail,
          5  -> Equipment.PlateArmor
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
