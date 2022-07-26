package eu.joaocosta.roguelike.savestate

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

import eu.joaocosta.roguelike._
import eu.joaocosta.roguelike.constants.Message
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components._
import eu.joaocosta.roguelike.entity.entities._

object Codecs {
  implicit lazy val fighterCodec: Codec[Fighter]     = deriveCodec
  implicit lazy val behaviorCodec: Codec[Behavior]   = deriveCodec
  implicit lazy val equipmentCodec: Codec[Equipment] = deriveCodec
  implicit lazy val equipmentMapCodec: Codec[Map[Equipable.Slot, Equipment]] =
    Codec.from(
      Decoder[List[Equipment]].map(_.map(x => x.slot -> x).toMap),
      Encoder[List[Equipment]].contramap(_.values.toList)
    )
  implicit lazy val itemCodec: Codec[Item] = deriveCodec
  implicit lazy val inventoryCodec: Codec[Inventory] = {
    implicit val consumableDecoder: Decoder[ConsumableEntity] =
      Decoder[Entity].emapTry(e => scala.util.Try(e.asInstanceOf[ConsumableEntity]))
    implicit val consumableEncoder: Encoder[ConsumableEntity] = Encoder[Entity].contramap(x => x)
    deriveCodec
  }
  implicit lazy val playerCodec: Codec[Player] = deriveCodec
  implicit lazy val npcCodec: Codec[Npc]       = deriveCodec
  implicit lazy val corpseCodec: Codec[Corpse] = deriveCodec
  implicit lazy val entityCodec: Codec[Entity] = {
    val decoder: Decoder[Entity] = Decoder.decodeJsonObject.emap { obj =>
      obj("type").toRight("Required field 'type' is missing").flatMap { typeJson =>
        obj("entity").toRight("Required field 'entity' is missing").flatMap { entityJson =>
          typeJson.asString.toRight("Entity type must be a String").flatMap {
            case "corpse"    => corpseCodec.decodeJson(entityJson).left.map(_.message)
            case "equipment" => equipmentCodec.decodeJson(entityJson).left.map(_.message)
            case "item"      => itemCodec.decodeJson(entityJson).left.map(_.message)
            case "npc"       => npcCodec.decodeJson(entityJson).left.map(_.message)
            case "player"    => playerCodec.decodeJson(entityJson).left.map(_.message)
            case t           => Left("Unsupported entity: " + t)
          }
        }
      }
    }

    val encoder: Encoder[Entity] = Encoder.instance {
      case corpse: Corpse       => JsonObject("type" := "corpse", "entity" := corpse).asJson
      case equipment: Equipment => JsonObject("type" := "equipment", "entity" := equipment).asJson
      case item: Item           => JsonObject("type" := "item", "entity" := item).asJson
      case npc: Npc             => JsonObject("type" := "npc", "entity" := npc).asJson
      case player: Player       => JsonObject("type" := "player", "entity" := player).asJson
      case e                    => throw RuntimeException("Unsuported entity: " + e)
    }

    Codec.from(decoder, encoder)
  }
  implicit lazy val coordinatesKeyEncoder: KeyEncoder[(Int, Int)] =
    KeyEncoder.instance { case (x, y) => x + "," + y }
  implicit lazy val coordinatesKeyDecoder: KeyDecoder[(Int, Int)] =
    KeyDecoder.instance(str =>
      str.split(",").flatMap(_.toIntOption).toList match {
        case x :: y :: Nil => Some((x, y))
        case _             => None
      }
    )
  implicit lazy val gameMapTileCodec: Codec[GameMap.Tile] = deriveCodec
  implicit lazy val gameMapCodec: Codec[GameMap]          = deriveCodec
  implicit lazy val messageCodec: Codec[Message]          = deriveCodec
  implicit lazy val gameStateCodec: Codec[GameState]      = deriveCodec
}
