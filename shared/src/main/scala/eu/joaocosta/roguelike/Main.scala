package eu.joaocosta.roguelike

import eu.joaocosta.minart.backend.defaults._
import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.minart.runtime._
import eu.joaocosta.minart.runtime.pure._
import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.rendering.AppStateRenderer

object Main extends MinartApp {

  type State = (AppState, Input)
  val loopRunner = LoopRunner()
  val canvasSettings = Canvas.Settings(
    width = constants.screenWidth * constants.spriteWidth,
    height = constants.screenHeight * constants.spriteHeight,
    scale = 2,
    clearColor = constants.Pallete.black,
    title = constants.title
  )
  val canvasManager = CanvasManager()
  val initialState  = (AppState.initialState, Input())
  val frameRate     = LoopFrequency.hz60
  val terminateWhen = (state: State) => state._1 == Leaving

  val renderFrame = { case (appState, input) =>
    for {
      _        <- CanvasIO.redraw
      newInput <- CanvasIO.getKeyboardInput.map(input.update)
      pointer  <- CanvasIO.getPointerInput
      _        <- CanvasIO.clear()
      _        <- AppStateRenderer.render(appState, Resources.richFont, pointer)
      actions   = Action.getActions(appState, input.keyPresses)
      nextState = (appState.applyActions(actions), newInput)
    } yield nextState
  }
}
