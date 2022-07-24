package eu.joaocosta.roguelike

import java.io.OutputStreamWriter

import scala.util.Try

import io.circe._
import io.circe.syntax._

import eu.joaocosta.minart.runtime.Resource
import eu.joaocosta.roguelike.savestate.Codecs._

package object savestate {
  def saveGame(resource: Resource, gameState: GameState): Try[Unit] = {
    val encodedState = gameState.asJson.noSpaces
    resource.withOutputStream { os =>
      val writer = new OutputStreamWriter(os)
      writer.write(encodedState)
      writer.flush()
    }
  }

  def loadGame(resource: Resource): Try[GameState] = {
    resource.withSource { source =>
      parser.parse(source.getLines().flatten.mkString("")).flatMap(_.as[GameState]).toTry
    }.flatten
  }
}
