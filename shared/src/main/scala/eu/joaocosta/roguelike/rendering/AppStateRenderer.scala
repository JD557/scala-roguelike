package eu.joaocosta.roguelike.rendering

import scala.annotation.tailrec
import scala.util.ChainingSyntax

import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.image._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.minart.input._
import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike._
import eu.joaocosta.roguelike.constants.Pallete
import eu.joaocosta.roguelike.entity._
import eu.joaocosta.roguelike.entity.components.Equipable.Slot
import eu.joaocosta.roguelike.entity.entities.Player

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
    val tileMap = (state.exploredTiles ++ state.visibleTiles).iterator.map { case pos =>
      if (state.visibleTiles(pos)) pos -> state.currentLevel.gameMap.tiles(pos).sprite
      else pos                         -> state.currentLevel.gameMap.tiles(pos).darkSprite
    }
    val (walkableEntities, nonWalkableEntities) = state.entities.iterator
      .filter(e => state.visibleTiles(e.x, e.y))
      .flatMap(e =>
        state.currentLevel.gameMap.tiles
          .get((e.x, e.y))
          .map(tile => e.isWalkable -> ((e.x, e.y) -> e.sprite.copy(bg = tile.sprite.bg)))
      )
      .partition(_._1)
    window.addTiles((tileMap ++ walkableEntities.map(_._2) ++ nonWalkableEntities.map(_._2)).to(Iterable))
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

  private def putPlayerStatus(state: GameState, showHelp: Boolean = false)(window: Window): Window = {
    def drawBar(text: String, filled: Int, total: Int, barSize: Int, filledColor: Color, emptyColor: Color): Window = {
      val filledTiles = (barSize * filled) / total
      val freeTiles   = barSize - filledTiles
      val barText     = (s" $text: $filled/$total").padTo(barSize, ' ').take(barSize)
      Window.empty
        .printLine(
          0,
          0,
          barText.take(filledTiles),
          Pallete.white,
          filledColor
        )
        .printLine(
          filledTiles,
          0,
          barText.drop(filledTiles),
          Pallete.white,
          emptyColor
        )
    }
    val hpBar = drawBar(
      "HP",
      state.player.fighter.hp,
      state.player.fighter.maxHp,
      constants.barSize,
      Pallete.green,
      Pallete.red
    )
    val expBar = drawBar(
      "EXP",
      state.player.exp,
      Player.nextLevel(state.player.level),
      constants.barSize,
      Pallete.blue,
      Pallete.darkBlue
    )
    window
      .putWindow(constants.leftStatusX, constants.statusY, hpBar)
      .printLine(
        constants.leftStatusX,
        constants.statusY + 1,
        s" ATK: ${state.player.fighter.attack}"
      )
      .printLine(
        constants.leftStatusX,
        constants.statusY + 2,
        s" DEF: ${state.player.fighter.defense}"
      )
      .printLine(
        constants.leftStatusX,
        constants.statusY + 3,
        s" ITEMS: ${state.player.inventory.items.size}/${state.player.inventory.capacity}"
      )
      .putWindow(constants.rightStatusX, constants.statusY, expBar)
      .printLine(
        constants.rightStatusX,
        constants.statusY + 1,
        s" LEVEL: ${state.player.level}"
      )
      .printLine(
        constants.rightStatusX,
        constants.statusY + 2,
        s" FLOOR: ${state.currentLevel.floor}"
      )
      .printLine(
        constants.rightStatusX,
        constants.statusY + 4,
        " F: Fullscreen"
      )
      .printLine(
        constants.rightStatusX,
        constants.statusY + 5,
        if (showHelp) " H: Show help" else ""
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
    val itemSubwindow = state.currentState.player.inventory.items.zipWithIndex
      .foldLeft(Window.empty) { case (window, (item, idx)) =>
        window
          .addTile((0, idx + 1), item.sprite.copy(bg = Pallete.black))
          .printLine(
            1,
            idx + 1,
            item.name,
            Pallete.white,
            if (state.cursor == idx) Pallete.gray else Pallete.black
          )
      }
    val weapon = state.currentState.player.fighter.equipment
      .get(Slot.Weapon)
      .fold("(NONE)")(w => s"${w.sprite.char}${w.name} (+ ${w.attackBonus} ATK)")
    val armor = state.currentState.player.fighter.equipment
      .get(Slot.Armor)
      .fold("(NONE)")(a => s"${a.sprite.char}${a.name} (+ ${a.defenseBonus} DEF)")
    val selectedSlot = state.selectedItem.flatMap {
      case Left(e) => Some(e.slot)
      case _       => None
    }
    val equipmentSubwindow = Window.empty
      .printLine(
        0,
        0,
        s"WEAPON: $weapon",
        Pallete.white,
        if (selectedSlot.contains(Slot.Weapon)) Pallete.gray else Pallete.black
      )
      .printLine(
        0,
        1,
        s"ARMOR: $armor",
        Pallete.white,
        if (selectedSlot.contains(Slot.Armor)) Pallete.gray else Pallete.black
      )

    window
      .pipe(
        addPopup(
          constants.popUpX,
          0,
          constants.popUpX + constants.popUpW,
          constants.screenHeight - 5,
          "Inventory",
          itemSubwindow
        )
      )
      .pipe(
        addPopup(
          constants.popUpX,
          constants.screenHeight - 4,
          constants.popUpX + constants.popUpW,
          constants.screenHeight - 1,
          "Equipment",
          equipmentSubwindow
        )
      )
  }

  private def printHelp(window: Window): Window = {
    val helpSubwindow =
      Window.empty
        .printLine(0, 0, s"""Welcome to "${constants.title}"""")
        .printLine(0, 2, "Explore the dungeon.")
        .printLine(0, 3, "Find loot.")
        .printLine(0, 4, "Beware of dangerous monsters.")
        .printLine(0, 5, "See how far down you can get!")
        .printLine(0, 7, "Press H to go back to the game.")
    val controlsSubwindow =
      Window.empty
        .printLine(0, 0, "In game:", Pallete.blue)
        .printLine(0, 1, "Arrows: Move/Attack")
        .printLine(0, 2, "Space: Wait")
        .printLine(0, 3, "G: Pick up item")
        .printLine(0, 4, "D: Go downstairs")
        .printLine(0, 5, "L: Look around")
        .printLine(0, 6, "I: View inventory")
        .printLine(0, 7, "I: View Log history")
        .printLine(0, 8, "Esc: Pause")
        .printLine(0, 9, "H: View help")
        .printLine(0, 11, "Look Around/Targeting:", Pallete.blue)
        .printLine(0, 12, "Arrows: Move Cursor")
        .printLine(0, 13, "Enter: Choose Target")
        .printLine(0, 14, "Esc: Go Back")
        .printLine(0, 16, "Inventory:", Pallete.blue)
        .printLine(0, 17, "Arrows: Move Cursor")
        .printLine(0, 18, "Enter: Use/Equip/Unequip")
        .printLine(0, 19, "Backspace: Drop")
        .printLine(0, 20, "Esc: Go Back")
        .printLine(0, 22, "Log History:", Pallete.blue)
        .printLine(0, 23, "Arrows: Scroll")
        .printLine(0, 24, "Esc: Go Back")

    window
      .pipe(
        addPopup(
          0,
          0,
          (constants.screenWidth / 2) - 1,
          constants.statusY - 2,
          "Help",
          helpSubwindow
        )
      )
      .pipe(
        addPopup(
          (constants.screenWidth / 2),
          0,
          constants.screenWidth - 1,
          constants.statusY - 2,
          "Controls",
          controlsSubwindow
        )
      )
  }

  def toWindow(state: AppState, pointerPos: Option[PointerInput.Position]): Window = state match {
    case Menu(cursor, message) =>
      val subwindow = Window.empty
        .printLine(0, 0, "New Game", Pallete.white, if (cursor == 0) Pallete.gray else Pallete.black)
        .printLine(0, 1, "Daily Challenge", Pallete.white, if (cursor == 1) Pallete.gray else Pallete.black)
        .printLine(0, 2, "Load Game", Pallete.white, if (cursor == 2) Pallete.gray else Pallete.black)
        .printLine(0, 3, "Quit Game", Pallete.white, if (cursor == 3) Pallete.gray else Pallete.black)

      val titleX   = (constants.screenWidth - constants.title.size) / 2
      val titleY   = constants.screenHeight / 2 - 4
      val messageX = (constants.screenWidth - message.getOrElse("").size) / 2
      Window.empty
        .printLine(titleX, titleY, constants.title, Pallete.red)
        .pipe(
          addPopup(titleX, titleY + 1, titleX + constants.title.size - 1, titleY + 6, "", subwindow)
        )
        .printLine(messageX, titleY + 7, message.getOrElse(""), Pallete.white)
    case Help(gameState) =>
      Window.empty
        .pipe(printHelp)
        .pipe(putGameMessages(gameState, constants.maxMessages, 0))
        .pipe(putPlayerStatus(gameState))
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
        .pipe(putPlayerStatus(inGame.gameState, showHelp = true))
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
    case LevelUp(gameState, cursor) =>
      val subwindow = Window.empty
        .printLine(
          0,
          0,
          s"Constitution (+${constants.hpBonus})",
          Pallete.white,
          if (cursor == 0) Pallete.gray else Pallete.black
        )
        .printLine(
          0,
          1,
          s"Attack (+${constants.attackBonus})",
          Pallete.white,
          if (cursor == 1) Pallete.gray else Pallete.black
        )
        .printLine(
          0,
          2,
          s"Defense (+${constants.defenseBonus})",
          Pallete.white,
          if (cursor == 2) Pallete.gray else Pallete.black
        )
      Window.empty
        .pipe(putGameTiles(gameState))
        .pipe(
          addPopup(
            constants.popUpX,
            constants.screenHeight - 6,
            constants.popUpX + constants.popUpW,
            constants.screenHeight - 1,
            "Level Up",
            subwindow
          )
        )
    case _ => Window.empty
  }
}
