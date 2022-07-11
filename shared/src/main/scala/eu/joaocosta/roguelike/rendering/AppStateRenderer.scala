package eu.joaocosta.roguelike.rendering

import scala.annotation.tailrec
import scala.util.ChainingSyntax

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.{AppState, GameMap, constants}

object AppStateRenderer extends ChainingSyntax {

  def render(state: AppState, tileset: SpriteSheet, pointer: PointerInput): CanvasIO[Unit] =
    toWindow(state, pointer.position).render(tileset)

  private def addPopup(x1: Int, y1: Int, x2: Int, y2: Int, title: String, subWindow: Window)(window: Window): Window = {
    window
      .putBorders(
        x1,
        y1,
        x2,
        y2,
        fg = Pallete.white,
        bg = Pallete.black
      )
      .printLine(
        x1 + 1,
        y1,
        title,
        fg = Pallete.black,
        bg = Pallete.white
      )
      .putWindow(x1 + 1, y1 + 1, subWindow)
  }

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

  private def wrapText(str: String, lineWidth: Int): List[String] = {
    @tailrec
    def loop(fit: String, dontFit: List[String]): (String, String) =
      (fit, dontFit) match {
        case (_, w :: ws) if fit.size + w.size + 1 <= lineWidth => loop(fit + " " + w, ws)
        case _                                                  => (fit, dontFit.mkString(" "))
      }
    if (str.isEmpty) Nil
    else {
      val words          = str.split(" ").toList
      val (fit, dontFit) = loop(words.head, words.tail)
      fit :: wrapText(dontFit, lineWidth)
    }
  }

  private def putGameMessages(state: InGame, limit: Int, scroll: Int)(window: Window): Window = {
    val charLimit = constants.popUpW - 1
    val wrappedMessages = state.messages.flatMap { case msg =>
      wrapText(msg.text, charLimit).map(txt => txt -> msg.color).reverse
    }
    val subwindow = wrappedMessages
      .drop(scroll)
      .take(limit)
      .zipWithIndex
      .foldLeft(Window.empty) { case (win, ((text, color), y)) =>
        win.printLine(0, limit - 1 - y, text, color)
      }
    addPopup(
      constants.popUpX,
      constants.screenHeight - 2 - limit,
      constants.popUpX + constants.popUpW,
      constants.screenHeight - 1,
      "Message Log",
      subwindow
    )(window)
  }

  private def putPlayerStatus(state: InGame)(window: Window): Window = {
    val filledTiles = (constants.hpBarSize * state.player.fighter.hp) / state.player.fighter.maxHp
    val freeTiles   = constants.hpBarSize - filledTiles
    val barText = s" HP: ${state.player.fighter.hp}/${state.player.fighter.maxHp}"
      .padTo(constants.hpBarSize, ' ')
      .take(constants.hpBarSize)
    window
      .printLine(
        0,
        constants.screenHeight - constants.maxMessages,
        barText.take(filledTiles),
        Pallete.white,
        Pallete.green
      )
      .printLine(
        filledTiles,
        constants.screenHeight - constants.maxMessages,
        barText.drop(filledTiles),
        Pallete.white,
        Pallete.red
      )
      .printLine(
        1,
        constants.screenHeight - constants.maxMessages + 1,
        s"ATK: ${state.player.fighter.attack}"
      )
      .printLine(
        11,
        constants.screenHeight - constants.maxMessages + 1,
        s"DEF: ${state.player.fighter.defense}"
      )
      .printLine(
        1,
        constants.screenHeight - constants.maxMessages + 2,
        s"CAP: ${state.player.inventory.items.size}/${state.player.inventory.capacity}"
      )
  }

  private def printSelectedEntities(state: InGame, pointerPos: Option[PointerInput.Position])(
      window: Window
  ): Window = {
    val selectedTile     = pointerPos.map(pos => (pos.x / constants.spriteWidth, pos.y / constants.spriteHeight))
    val selectedEntities = state.entities.filter(e => selectedTile.exists { case (px, py) => px == e.x && py == e.y })
    selectedTile
      .fold(window) { case (px, py) => window.invertColors(px, py) }
      .printLine(1, constants.screenHeight - 1, selectedEntities.map(_.name).mkString(", "))
  }

  private def printInventory(state: InventoryView)(
      window: Window
  ): Window = {
    val subwindow = state.currentState.player.inventory.items.zipWithIndex
      .foldLeft(Window.empty) { case (window, (item, idx)) =>
        window.printLine(
          0,
          idx + 1,
          item.name,
          Pallete.white,
          if (state.cursor == idx) Pallete.darkGray else Pallete.black
        )
      }
    addPopup(
      constants.popUpX,
      0,
      constants.popUpX + constants.popUpW,
      constants.screenHeight - 1,
      "Inventory",
      subwindow
    )(window)
  }

  def toWindow(state: AppState, pointerPos: Option[PointerInput.Position]): Window = state match {
    case inGame: InGame =>
      Window.empty
        .pipe(putGameTiles(inGame))
        .pipe(printSelectedEntities(inGame, pointerPos))
        .pipe(putGameMessages(inGame, constants.maxMessages, 0))
        .pipe(putPlayerStatus(inGame))
    case gameOver: GameOver =>
      toWindow(gameOver.finalState, pointerPos)
    case historyView: HistoryView =>
      Window.empty
        .pipe(putGameTiles(historyView.currentState))
        .pipe(putGameMessages(historyView.currentState, constants.screenHeight - 2, historyView.scroll))
        .pipe(putPlayerStatus(historyView.currentState))
    case inventoryView: InventoryView =>
      Window.empty
        .pipe(putGameTiles(inventoryView.currentState))
        .pipe(printInventory(inventoryView))
        .pipe(putPlayerStatus(inventoryView.currentState))
    case _ => Window.empty
  }
}
