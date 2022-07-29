package eu.joaocosta.roguelike

import eu.joaocosta.minart.backend.defaults._
import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.minart.runtime._
import eu.joaocosta.minart.runtime.pure._
import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.rendering.AppStateRenderer

object Main extends MinartApp {
  type State = AppState
  val loopRunner = LoopRunner()
  val canvasSettings = Canvas.Settings(
    width = constants.screenWidth * constants.spriteWidth,
    height = constants.screenHeight * constants.spriteHeight,
    scale = 2,
    clearColor = constants.Pallete.black
  )
  val canvasManager = CanvasManager()
  val initialState  = AppState.initialState
  val frameRate     = LoopFrequency.Uncapped
  val terminateWhen = (state: AppState) => state == Leaving

  val renderFrame = (appState: AppState) =>
    for {
      _       <- CanvasIO.redraw
      input   <- CanvasIO.getKeyboardInput
      pointer <- CanvasIO.getPointerInput
      _       <- CanvasIO.clear()
      _       <- AppStateRenderer.render(appState, Resources.richFont, pointer)
      actions   = Action.getActions(appState, input)
      nextState = appState.applyActions(actions)
    } yield nextState
}
