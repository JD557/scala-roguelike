package eu.joaocosta.roguelike

import eu.joaocosta.roguelike.entity.entities.Item.FireballScroll
import eu.joaocosta.roguelike.entity.entities.{Item, Npc}
import eu.joaocosta.roguelike.random.Distribution

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
    maxMonsters = {
      case floor if floor < 4 => 2
      case floor if floor < 6 => 3
      case _                  => 5
    },
    monsterDistribution = {
      case floor if floor < 3 => Distribution.weighted(80 -> (Npc.Orc(_, _)))
      case floor if floor < 5 => Distribution.weighted(80 -> (Npc.Orc(_, _)), 15 -> (Npc.Troll(_, _)))
      case floor if floor < 7 => Distribution.weighted(80 -> (Npc.Orc(_, _)), 30 -> (Npc.Troll(_, _)))
      case _                  => Distribution.weighted(80 -> (Npc.Orc(_, _)), 60 -> (Npc.Troll(_, _)))
    },
    maxItems = {
      case floor if floor < 4 => 1
      case _                  => 2
    },
    itemDistribution = {
      case floor if floor < 2 => Distribution.weighted(35 -> (Item.HealingPotion(_, _)))
      case floor if floor < 4 =>
        Distribution.weighted(35 -> (Item.HealingPotion(_, _)), 10 -> (Item.ConfusionScroll(_, _)))
      case floor if floor < 6 =>
        Distribution.weighted(
          35 -> (Item.HealingPotion(_, _)),
          10 -> (Item.ConfusionScroll(_, _)),
          25 -> (Item.LightningScroll(_, _))
        )
      case _ =>
        Distribution.weighted(
          35 -> (Item.HealingPotion(_, _)),
          10 -> (Item.ConfusionScroll(_, _)),
          25 -> (Item.LightningScroll(_, _)),
          25 -> (Item.FireballScroll(_, _))
        )
    }
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
