package eu.joaocosta.roguelike.rendering

import scala.util.ChainingSyntax

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.{AppState, Constants, GameMap}

object AppStateRenderer extends ChainingSyntax {

  def render(state: AppState, tileset: SpriteSheet, pointer: PointerInput): CanvasIO[Unit] =
    toWindow(state, pointer.position).render(tileset)

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
      .toList
      .sortBy(!_._1.isWalkable)
      .map { case (entity, tile) =>
        (entity.x, entity.y) -> entity.sprite.copy(bg = tile.bg)
      }
    window.addTiles(tileMap).addTiles(entitySprites)
  }

  private def putGameMessages(state: InGame, limit: Int, scroll: Int)(window: Window): Window = {
    val borders =
      window
        .addBorders(
          Constants.popUpX,
          Constants.screenHeight - 2 - limit,
          Constants.popUpX + Constants.popUpW,
          Constants.screenHeight - 1
        )
        .printLine(Constants.popUpX + 1, Constants.screenHeight - 2 - limit, "Message Log")
    state.messages
      .drop(scroll)
      .take(limit)
      .zipWithIndex
      .foldLeft(borders) { case (win, (message, y)) =>
        win.printLine(Constants.popUpX + 1, Constants.screenHeight - 2 - y, message.text, message.color)
      }
  }

  private def putPlayerStatus(state: InGame)(window: Window): Window = {
    val filledTiles = (Constants.hpBarSize * state.player.fighter.hp) / state.player.fighter.maxHp
    val freeTiles   = Constants.hpBarSize - filledTiles
    val barText = s" HP: ${state.player.fighter.hp}/${state.player.fighter.maxHp}"
      .padTo(Constants.hpBarSize, ' ')
      .take(Constants.hpBarSize)
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
        s"ATK: ${state.player.fighter.attack}"
      )
      .printLine(
        11,
        Constants.screenHeight - Constants.maxMessages + 1,
        s"DEF: ${state.player.fighter.defense}"
      )
      .printLine(
        1,
        Constants.screenHeight - Constants.maxMessages + 2,
        s"CAP: ${state.player.inventory.items.size}/${state.player.inventory.capacity}"
      )
  }

  private def printSelectedEntities(state: InGame, pointerPos: Option[PointerInput.Position])(
      window: Window
  ): Window = {
    val selectedTile     = pointerPos.map(pos => (pos.x / Constants.spriteWidth, pos.y / Constants.spriteHeight))
    val selectedEntities = state.entities.filter(e => selectedTile.exists { case (px, py) => px == e.x && py == e.y })
    selectedTile
      .fold(window) { case (px, py) => window.invertColors(px, py) }
      .printLine(1, Constants.screenHeight - 1, selectedEntities.map(_.name).mkString(", "))
  }

  private def printInventory(state: InventoryView)(
      window: Window
  ): Window = {
    val borders = window
      .addBorders(
        Constants.popUpX,
        0,
        Constants.popUpX + Constants.popUpW,
        Constants.screenHeight - 1
      )
      .printLine(Constants.popUpX + 1, 0, "Inventory")
    state.currentState.player.inventory.items.zipWithIndex
      .foldLeft(borders) { case (window, (item, idx)) =>
        window.printLine(
          Constants.popUpX + 1,
          idx + 1,
          item.name,
          Constants.Pallete.white,
          if (state.cursor == idx) Constants.Pallete.darkGray else Constants.Pallete.black
        )
      }
  }

  def toWindow(state: AppState, pointerPos: Option[PointerInput.Position]): Window = state match {
    case inGame: InGame =>
      Window.empty
        .pipe(putGameTiles(inGame))
        .pipe(printSelectedEntities(inGame, pointerPos))
        .pipe(putGameMessages(inGame, Constants.maxMessages, 0))
        .pipe(putPlayerStatus(inGame))
    case gameOver: GameOver =>
      toWindow(gameOver.finalState, pointerPos)
    case historyView: HistoryView =>
      Window.empty
        .pipe(putGameMessages(historyView.currentState, Constants.screenHeight - 2, historyView.scroll))
        .pipe(putPlayerStatus(historyView.currentState))
    case inventoryView: InventoryView =>
      Window.empty
        .pipe(printInventory(inventoryView))
        .pipe(putPlayerStatus(inventoryView.currentState))
    case _ => Window.empty
  }
}
