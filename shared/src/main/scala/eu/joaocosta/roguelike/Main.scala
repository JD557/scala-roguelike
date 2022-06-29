package eu.joaocosta.roguelike

import eu.joaocosta.minart.backend.defaults._
import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.minart.runtime._
import eu.joaocosta.minart.runtime.pure._

object Main extends MinartApp {
  type State = AppState
  val loopRunner = LoopRunner()
  val canvasSettings = Canvas.Settings(
    width = Constants.screenWidth * Constants.spriteWidth,
    height = Constants.screenHeight * Constants.spriteHeight,
    scale = 2,
    clearColor = Constants.Pallete.black
  )
  val canvasManager = CanvasManager()
  val initialState  = AppState.initialState
  val frameRate     = LoopFrequency.Uncapped
  val terminateWhen = (state: AppState) => state == Leaving
  val renderFrame = (appState: AppState) =>
    appState match {
      case Leaving => CanvasIO.pure(appState)
      case state: InGame =>
        for {
          _     <- CanvasIO.redraw
          input <- CanvasIO.getKeyboardInput
          _     <- CanvasIO.clear()
          _     <- state.toWindow.render(Resources.richFont)
          actions   = Action.getActions(input)
          nextState = actions.foldLeft(appState)((st, a) => st.applyAction(a))
        } yield nextState
    }
}
