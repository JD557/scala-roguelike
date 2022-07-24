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
import eu.joaocosta.roguelike.{AppState, GameMap, GameState, constants}

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

  private def putGameTiles(state: GameState)(window: Window): Window = {
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

  private def putGameMessages(state: GameState, limit: Int, scroll: Int)(window: Window): Window = {
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

  private def putPlayerStatus(state: GameState)(window: Window): Window = {
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

  private def printSelectedEntities(state: GameState, cursorPos: Option[(Int, Int, Int)])(
      window: Window
  ): Window = {
    val selectedTiles = cursorPos.toList.flatMap { case (cx, cy, radius) =>
      for {
        dx <- (-radius to radius)
        dy <- (-radius to radius)
      } yield (cx + dx, cy + dy)
    }.toSet
    val selectedEntities = state.entities.filter(e => selectedTiles((e.x, e.y)))
    selectedTiles
      .foldLeft(window) { case (w, (px, py)) =>
        if (w.tiles.contains(px, py)) w.invertColors(px, py)
        else w.addTile((px, py) -> Window.Sprite('.', Pallete.gray, Pallete.black))
      }
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
          if (state.cursor == idx) Pallete.gray else Pallete.black
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
    case Menu(cursor, message) =>
      val subwindow = Window.empty
        .printLine(0, 0, "New Game", Pallete.white, if (cursor == 0) Pallete.gray else Pallete.black)
        .printLine(0, 1, "Load Game", Pallete.white, if (cursor == 1) Pallete.gray else Pallete.black)
        .printLine(0, 2, "Quit Game", Pallete.white, if (cursor == 2) Pallete.gray else Pallete.black)

      val titleX   = (constants.screenWidth - constants.title.size) / 2
      val titleY   = constants.screenHeight / 2 - 4
      val messageX = (constants.screenWidth - message.getOrElse("").size) / 2
      Window.empty
        .printLine(titleX, titleY, constants.title, Pallete.red)
        .pipe(
          addPopup(titleX, titleY + 1, titleX + constants.title.size - 1, titleY + 5, "", subwindow)
        )
        .printLine(messageX, titleY + 6, message.getOrElse(""), Pallete.white)
    case Pause(gameState, cursor) =>
      val subwindow = Window.empty
        .printLine(0, 0, "Continue", Pallete.white, if (cursor == 0) Pallete.gray else Pallete.black)
        .printLine(0, 1, "Save Game", Pallete.white, if (cursor == 1) Pallete.gray else Pallete.black)
        .printLine(0, 2, "Back to Menu", Pallete.white, if (cursor == 2) Pallete.gray else Pallete.black)
        .printLine(0, 3, "Quit", Pallete.white, if (cursor == 3) Pallete.gray else Pallete.black)
      Window.empty
        .pipe(putGameTiles(gameState))
        .pipe(
          addPopup(
            constants.popUpX,
            constants.screenHeight - 6,
            constants.popUpX + constants.popUpW,
            constants.screenHeight - 1,
            "Pause",
            subwindow
          )
        )
    case inGame: InGame =>
      val cursorPos = pointerPos.map(pos => (pos.x / constants.spriteWidth, pos.y / constants.spriteHeight, 0))
      Window.empty
        .pipe(putGameTiles(inGame.gameState))
        .pipe(printSelectedEntities(inGame.gameState, cursorPos))
        .pipe(putGameMessages(inGame.gameState, constants.maxMessages, 0))
        .pipe(putPlayerStatus(inGame.gameState))
    case gameOver: GameOver =>
      val cursorPos = pointerPos.map(pos => (pos.x / constants.spriteWidth, pos.y / constants.spriteHeight, 0))
      Window.empty
        .pipe(putGameTiles(gameOver.finalState))
        .pipe(printSelectedEntities(gameOver.finalState, cursorPos))
        .pipe(putGameMessages(gameOver.finalState, constants.maxMessages, 0))
        .pipe(putPlayerStatus(gameOver.finalState))
    case lookAround: LookAround =>
      Window.empty
        .pipe(putGameTiles(lookAround.currentState))
        .pipe(
          printSelectedEntities(
            lookAround.currentState,
            Some((lookAround.cursorX, lookAround.cursorY, lookAround.radius))
          )
        )
        .pipe(putPlayerStatus(lookAround.currentState))
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
