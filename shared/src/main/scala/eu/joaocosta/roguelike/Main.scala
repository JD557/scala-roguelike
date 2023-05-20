package eu.joaocosta.roguelike

import eu.joaocosta.minart.backend.defaults._
import eu.joaocosta.minart.graphics._
import eu.joaocosta.minart.graphics.pure._
import eu.joaocosta.minart.input.KeyboardInput
import eu.joaocosta.minart.runtime._
import eu.joaocosta.minart.runtime.pure._
import eu.joaocosta.roguelike.AppState._
import eu.joaocosta.roguelike.rendering.AppStateRenderer

object Main extends MinartApp[(AppState, Input), LowLevelCanvas] {

  val loopRunner      = LoopRunner()
  val createSubsystem = () => LowLevelCanvas.create()
  val canvasSettings = Canvas.Settings(
    width = constants.screenWidth * constants.spriteWidth,
    height = constants.screenHeight * constants.spriteHeight,
    scale = Some(2),
    clearColor = constants.Pallete.black,
    title = constants.title
  )
  val initialState = (AppState.initialState, Input())
  val frameRate    = LoopFrequency.hz60

  val toggleFullscreen =
    CanvasIO.canvasSettings.flatMap(settings =>
      CanvasIO.changeSettings(settings.copy(fullScreen = !settings.fullScreen))
    )

  val appLoop = AppLoop
    .statefulRenderLoop[(AppState, Input)](
      { case (appState: AppState, input: Input) =>
        for {
          _        <- CanvasIO.redraw
          newInput <- CanvasIO.getKeyboardInput.map(input.update)
          pointer  <- CanvasIO.getPointerInput
          _        <- CanvasIO.clear()
          _        <- AppStateRenderer.render(appState, Resources.richFont, pointer)
          actions   = Action.getActions(appState, input.keyPresses)
          nextState = (appState.applyActions(actions), newInput)
          _ <- CanvasIO.when(input.keyPresses.contains(KeyboardInput.Key.F))(toggleFullscreen)
        } yield nextState
      },
      _._1 == AppState.Leaving
    )
    .configure(canvasSettings, frameRate, initialState)
}
