package eu.joaocosta.roguelike.rendering

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.{AppState, Constants, GameMap}

object AppStateRenderer {

  def render(state: AppState, tileset: SpriteSheet): CanvasIO[Unit] = toWindow(state).render(tileset)

  def toWindow(state: AppState): Window = state match {
    case inGame: InGame =>
      def tileSprite(pos: (Int, Int), tile: GameMap.Tile): Option[Window.Sprite] =
        if (inGame.visibleTiles(pos)) Some(tile.sprite)
        else if (inGame.exploredTiles(pos)) Some(tile.darkSprite)
        else None
      val tileMap = inGame.currentLevel.gameMap.tiles.flatMap { case (pos, tile) =>
        tileSprite(pos, tile).map(s => pos -> s)
      }.toMap
      val entitySprites = inGame.entities.iterator
        .filter(e => inGame.visibleTiles(e.x, e.y))
        .flatMap(e => tileMap.get(e.x, e.y).map(t => e -> t))
        .map { case (entity, tile) =>
          (entity.x, entity.y) -> entity.sprite.copy(bg = tile.bg)
        }
        .toList
      val baseWindow = Window.empty.addTiles(tileMap).addTiles(entitySprites)
      inGame.messages.zipWithIndex
        .foldLeft(baseWindow) { case (window, (textMessage, y)) =>
          val color =
            if (y == 0) Constants.Pallete.white
            else if (y == 1) Constants.Pallete.gray
            else Constants.Pallete.darkGray
          window.printLine(0, Constants.screenHeight - 2 - y, textMessage, color)
        }
        .printLine(0, Constants.screenHeight - 1, inGame.player.fighter.map(_.statusString).getOrElse(""))
    case _ => Window.empty
  }
}
