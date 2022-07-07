package eu.joaocosta.roguelike.rendering

import scala.util.ChainingSyntax

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.entity.Fighter
import eu.joaocosta.roguelike.{AppState, Constants, GameMap}

object AppStateRenderer extends ChainingSyntax {

  def render(state: AppState, tileset: SpriteSheet): CanvasIO[Unit] = toWindow(state).render(tileset)

  private def putGameTiles(state: InGame)(window: Window): Window = {
    def tileSprite(pos: (Int, Int), tile: GameMap.Tile): Option[Window.Sprite] =
      if (state.visibleTiles(pos)) Some(tile.sprite)
      else if (state.exploredTiles(pos)) Some(tile.darkSprite)
      else None
    val tileMap = state.currentLevel.gameMap.tiles.flatMap { case (pos, tile) =>
      tileSprite(pos, tile).map(s => pos -> s)
    }.toMap
    val entitySprites = state.entities.iterator
      .filter(e => state.visibleTiles(e.x, e.y))
      .flatMap(e => tileMap.get(e.x, e.y).map(t => e -> t))
      .map { case (entity, tile) =>
        (entity.x, entity.y) -> entity.sprite.copy(bg = tile.bg)
      }
      .toList
    window.addTiles(tileMap).addTiles(entitySprites)
  }

  private def putGameMessages(state: InGame)(window: Window): Window = {
    state.messages.zipWithIndex
      .foldLeft(window) { case (win, (message, y)) =>
        win.printLine(Constants.hpBarSize + 1, Constants.screenHeight - 1 - y, message.text, message.color)
      }
  }

  private def putPlayerStatus(state: InGame)(window: Window): Window = {
    val fighter     = state.player.fighter.getOrElse(Fighter(1, 1, 0, 0))
    val filledTiles = (Constants.hpBarSize * fighter.hp) / fighter.maxHp
    val freeTiles   = Constants.hpBarSize - filledTiles
    val barText     = s" HP: ${fighter.hp}/${fighter.maxHp}".padTo(Constants.hpBarSize, ' ').take(Constants.hpBarSize)
    window
      .printLine(
        0,
        Constants.screenHeight - Constants.maxMessages,
        barText.take(filledTiles),
        Constants.Pallete.white,
        Constants.Pallete.green
      )
      .printLine(
        filledTiles,
        Constants.screenHeight - Constants.maxMessages,
        barText.drop(filledTiles),
        Constants.Pallete.white,
        Constants.Pallete.red
      )
      .printLine(
        1,
        Constants.screenHeight - Constants.maxMessages + 1,
        s"ATK: ${fighter.attack}"
      )
      .printLine(
        1,
        Constants.screenHeight - Constants.maxMessages + 2,
        s"DEF: ${fighter.defense}"
      )
  }

  def toWindow(state: AppState): Window = state match {
    case inGame: InGame =>
      Window.empty.pipe(putGameMessages(inGame)).pipe(putGameTiles(inGame)).pipe(putPlayerStatus(inGame))
    case _ => Window.empty
  }
}
